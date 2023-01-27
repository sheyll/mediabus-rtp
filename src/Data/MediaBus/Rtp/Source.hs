{-# OPTIONS -Wno-unused-top-binds #-}

-- | A conduit 'Source' that receives bytestrings and parses RTP packets.
module Data.MediaBus.Rtp.Source (
  type RtpStream,
  convertRtpTimestampC, -- TODO Test
  rtpParserC,
  udpRtpSourceC, -- TODO Test
  rtpPayloadDemux,
  type RtpPayloadHandler,
  rtpPayloadDispatcher,
  type RtpPayloadConsumer,
)
where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as B
import Data.Default
import qualified Data.List
import Data.Maybe
import Data.MediaBus
import Data.MediaBus.Rtp.InternalLogging
import Data.MediaBus.Rtp.Packet
import Text.Printf
import Data.Proxy
import Network.Socket
import Data.Conduit.Network

{- | A handy alias for a 'Stream' for RTP packets, with according sequence
number, time stamp and payload types.
-}
type RtpStream p = Stream RtpSsrc RtpSeqNum RtpTimestamp p RtpPayload

data RRState ctx = MkRRState
  { _currCtx :: ctx
  , _isFirstPacket :: Bool
  }
  deriving (Show)

makeLenses ''RRState


-- | Opend a UDP port and listen for RTP packets.
udpRtpSourceC ::
  (MonadLogger m, MonadResource m) =>
  Int ->
  HostPreference ->
  ConduitT
    ()
    (RtpStream ())
    m
    ()
udpRtpSourceC !udpListenPort !udpListenIP  =
  annotateTypeSource
    (Proxy :: Proxy (Stream (SourceId (Maybe SockAddr)) RtpSeqNum (ClockTimeDiff UtcClock) () B.ByteString))
    (udpDatagramSource useUtcClock udpListenPort udpListenIP)
    .| rtpParserC


-- | A 'Conduit' that consumes 'ByteString's and produces an 'RtpStream'
rtpParserC ::
  ( Show i
  , Show s
  , Show t
  , Show p
  , Default i
  , Monad m
  , Default p
  , MonadLogger m
  ) =>
  ConduitT (Stream i s t p B.ByteString) (RtpStream p) m ()
rtpParserC =
  evalStateC (MkRRState (MkFrameCtx def def def def) True) $
    awaitForever processFrames
 where
  processFrames frm@(MkStream (Start _)) =
    out ("start frame received: " ++ show frm)
  processFrames (MkStream (Next (MkFrame _ _ !contentIn))) =
    case deserialize contentIn of
      Left rtpError -> logRtpError rtpError
      Right rtpPacket -> do
        let rtpHeader = header rtpPacket
        res <- updateState rtpHeader
        unless (res == FrameCtxNotChanged) (yieldStreamStart res)
        yieldStreamNext (body rtpPacket)
  logRtpError e = do
    ctx <- use currCtx
    err (printf "rtp packet parse error: %s, frame-context: %s" e (show ctx))
  updateState rtpHeader = do
    oldCtx <-
      currCtx
        <<%= ( (frameCtxSeqNumRef .~ sequenceNumber rtpHeader)
                . (frameCtxTimestampRef .~ headerTimestamp rtpHeader)
             )
    wasFirstPacket <- isFirstPacket <<.= False
    if oldCtx ^. frameCtxSourceId /= ssrc rtpHeader
      then do
        currCtx . frameCtxSourceId .= ssrc rtpHeader
        return RtpSsrcChanged
      else
        if sequenceNumbersDifferTooMuch
          (oldCtx ^. frameCtxSeqNumRef)
          (sequenceNumber rtpHeader)
          then return RtpSequenceNumberGap
          else
            if timestampsDifferTooMuch
              (oldCtx ^. frameCtxTimestampRef)
              (headerTimestamp rtpHeader)
              then return RtpTimestampGap
              else
                if wasFirstPacket
                  then return NewRtpSession
                  else return FrameCtxNotChanged
   where
    sequenceNumbersDifferTooMuch oldSN currSN =
      let d =
            if currSN >= oldSN
              then currSN - oldSN
              else oldSN - currSN -- TODO use LocalOrd??
          sequenceNumberMaxDelta = 10
       in d >= sequenceNumberMaxDelta
    timestampsDifferTooMuch oldTS currTS =
      let d =
            if currTS >= oldTS
              then currTS - oldTS
              else oldTS - currTS
          timestampMaxDelta = 2000 -- TODO extract
       in d >= timestampMaxDelta
  yieldStreamStart why = do
    fx <- use currCtx
    dbg (show why ++ ": starting rtp stream: " ++ show fx)
    yieldStartFrameCtx fx
  yieldStreamNext p = do
    ts <- use (currCtx . frameCtxTimestampRef)
    sn <- use (currCtx . frameCtxSeqNumRef)
    yield (MkStream (Next (MkFrame ts sn p)))

-- | The stream might re-'Start' for these reasons:
data RRSourceChange
  = NewRtpSession
  | RtpSsrcChanged
  | RtpSequenceNumberGap
  | RtpTimestampGap
  | FrameCtxNotChanged
  deriving (Eq, Show)

convertRtpTimestampC ::
  (Monad m, Num t) => ConduitT (Stream a b RtpTimestamp c d) (Stream a b (Ticks r t) c d) m ()
convertRtpTimestampC =
  mapC (timestamp %~ (MkTicks . fromIntegral . _rtpTimestamp))

{- | A utility that call the right 'RtpPayloadHandler' for the 'RtpPayloadType'
of the 'Frame'.
-}
rtpPayloadDemux ::
  (Integral t, Monad m) =>
  [(RtpPayloadType, RtpPayloadHandler (Ticks r t) c)] ->
  c ->
  ConduitT (RtpStream p) (Stream RtpSsrc RtpSeqNum (Ticks r t) p c) m ()
rtpPayloadDemux payloadTable fallbackContent = awaitForever go
 where
  go (MkStream (Next !frm)) = do
    let !pt = frm ^. framePayload . rtpPayloadType
    yieldNextFrame $
      case Data.List.lookup pt payloadTable of
        Just !handler ->
          handler frm
        Nothing ->
          frm
            & timestamp %~ (MkTicks . fromIntegral . _rtpTimestamp)
            & framePayload .~ fallbackContent
  go (MkStream (Start !frmCtx)) =
    yieldStartFrameCtx
      (frmCtx & timestamp %~ (MkTicks . fromIntegral . _rtpTimestamp))

{- | Functions from 'Frame' to 'Frame' for converting/coercing an 'RtpPayload'
to some common media type. NOTE: This is not for transcoding, this is rather
for /casting/ or /coercing/
-}
type RtpPayloadHandler t c = Frame RtpSeqNum RtpTimestamp RtpPayload -> Frame RtpSeqNum t c

-- | Send the RTP packets to the consumers registered to the payload type.
rtpPayloadDispatcher ::
  (Integral t, Monad m) =>
  [(RtpPayloadType, RtpPayloadConsumer p (Ticks r t) c m)] ->
  ConduitT (RtpStream p) (Stream RtpSsrc RtpSeqNum (Ticks r t) p c) m ()
rtpPayloadDispatcher payloadTable = awaitForever go
 where
  go (MkStream (Next !frameIn)) = do
    let pt = frameIn ^. framePayload . rtpPayloadType
        mHandler = Data.List.lookup pt payloadTable
    mapM_ (yieldNextFrame frameIn .|) mHandler
  go (MkStream (Start !frmCtx)) =
    yieldStartFrameCtx (frmCtx & timestamp %~ (MkTicks . fromIntegral . _rtpTimestamp))

type RtpPayloadConsumer p t c m =
  ConduitT (Stream RtpSsrc RtpSeqNum RtpTimestamp p RtpPayload) (Stream RtpSsrc RtpSeqNum t p c) m ()
