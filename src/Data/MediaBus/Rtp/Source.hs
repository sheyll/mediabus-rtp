{-# OPTIONS -Wno-unused-top-binds #-}

-- | A conduit 'Source' that receives bytestrings and parses RTP packets.
module Data.MediaBus.Rtp.Source
  ( type RtpStream,
    rtpSource,
    rtpPayloadDemux,
    type RtpPayloadHandler,
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

-- | A handy alias for a 'Stream' for RTP packets, with according sequence
-- number, time stamp and payload types.
type RtpStream p = Stream RtpSsrc RtpSeqNum RtpTimestamp p RtpPayload

data RRState ctx = MkRRState
  { _currCtx :: ctx,
    _isFirstPacket :: Bool
  }
  deriving (Show)

makeLenses ''RRState

-- | A 'Conduit' that consumes 'ByteString's and produces an 'RtpStream'
rtpSource ::
  ( Show i,
    Show s,
    Show t,
    Show p,
    Default i,
    Monad m,
    Default p,
    MonadLogger m
  ) =>
  ConduitT (Stream i s t p B.ByteString) (RtpStream p) m ()
rtpSource =
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

-- | A utility that call the right 'RtpPayloadHandler' for the 'RtpPayloadType'
-- of the 'Frame'.
rtpPayloadDemux ::
  (Integral t, Monad m) =>
  [(RtpPayloadType, RtpPayloadHandler (Ticks r t) c)] ->
  c ->
  ConduitT (RtpStream p) (Stream RtpSsrc RtpSeqNum (Ticks r t) p c) m ()
rtpPayloadDemux payloadTable fallbackContent =
  mapC (timestamp %~ (MkTicks . fromIntegral . _rtpTimestamp))
    .| awaitForever go
  where
    setFallbackContent = framePayload .~ fallbackContent
    go (MkStream (Next !frm)) = do
      let pt = frm ^. framePayload . rtpPayloadType
          mHandler = Data.List.lookup pt payloadTable
          !frm' = fromMaybe setFallbackContent mHandler frm
      yieldNextFrame frm'
    go (MkStream (Start !frmCtx)) = yieldStartFrameCtx frmCtx

-- | Functions from 'Frame' to 'Frame' for converting/coercing an 'RtpPayload'
-- to some common media type. NOTE: This is not for transcoding, this is rather
-- for /casting/ or /coercing/
type RtpPayloadHandler t c = Frame RtpSeqNum t RtpPayload -> Frame RtpSeqNum t c
