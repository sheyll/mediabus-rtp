{-# OPTIONS -Wno-unused-top-binds #-}

module Data.MediaBus.Rtp.Source
    ( type RtpStream
    , rtpSource
    , rtpPayloadDemux
    , type RtpPayloadHandler
    ) where

import           Conduit
import           Control.Lens
import qualified Data.ByteString          as B
import           Data.MediaBus
import           Data.MediaBus.Rtp.Packet
import           Control.Monad
import           Data.Default
import           Text.Printf
import           Debug.Trace
import qualified Data.List
import           Data.Maybe

type RtpStream p = Stream RtpSsrc RtpSeqNum RtpTimestamp p RtpPayload

data RRState ctx = MkRRState { _currCtx       :: ctx
                             , _isFirstPacket :: Bool
                             }
    deriving (Show)

makeLenses ''RRState

rtpSource :: (Default i, Monad m, Default p, Show p)
          => Conduit (Stream i s t p B.ByteString) m (RtpStream p)
rtpSource = evalStateC (MkRRState (MkFrameCtx def def def def) True) $
    awaitForever processFrames
  where
    processFrames (MkStream (Start _)) =
        return ()
    processFrames (MkStream (Next (MkFrame _ _ !contentIn))) =
        case deserialize contentIn of
            Left rtpError -> traceRtpError rtpError
            Right rtpPacket -> do
                let rtpHeader = header rtpPacket
                res <- updateState rtpHeader
                when (res == FrameCtxChanged) yieldStreamStart
                yieldStreamNext (body rtpPacket)

    traceRtpError e = do
        ctx <- use currCtx
        traceM (printf "RTP-ERROR:%s  Ctx: %s\n" e (show ctx))

    updateState rtpHeader = do
        oldCtx <- currCtx <<%=
                      ((frameCtxSeqNumRef .~ sequenceNumber rtpHeader)
                           . (frameCtxTimestampRef .~
                                  headerTimestamp rtpHeader))
        wasFirstPacket <- isFirstPacket <<.= False
        if oldCtx ^. frameCtxSourceId /= ssrc rtpHeader
            then do
                currCtx . frameCtxSourceId .= ssrc rtpHeader
                return FrameCtxChanged
            else if sequenceNumbersDifferTooMuch (oldCtx ^. frameCtxSeqNumRef)
                                                 (sequenceNumber rtpHeader) ||
                     timestampsDifferTooMuch (oldCtx ^. frameCtxTimestampRef)
                                             (headerTimestamp rtpHeader) ||
                     wasFirstPacket
                 then return FrameCtxChanged
                 else return FrameCtxNotChanged
      where
        sequenceNumbersDifferTooMuch oldSN currSN =
            let d = if currSN >= oldSN then currSN - oldSN else oldSN - currSN -- TODO use LocalOrd??
                sequenceNumberMaxDelta =
                    10
            in
                d >= sequenceNumberMaxDelta
        timestampsDifferTooMuch oldTS currTS =
            let d = if currTS >= oldTS then currTS - oldTS else oldTS - currTS
                timestampMaxDelta = 2000 -- TODO extract
            in
                d >= timestampMaxDelta
    yieldStreamStart = use currCtx >>= yieldStartFrameCtx
    yieldStreamNext p = do
        ts <- use (currCtx . frameCtxTimestampRef)
        sn <- use (currCtx . frameCtxSeqNumRef)
        yield (MkStream (Next (MkFrame ts sn p)))

data RRSourceChange = FrameCtxChanged | FrameCtxNotChanged
    deriving (Eq)

rtpPayloadDemux :: (Integral t, Monad m)
                => [(RtpPayloadType, RtpPayloadHandler (Ticks r t) c)]
                -> c
                -> Conduit (RtpStream p) m (Stream RtpSsrc RtpSeqNum (Ticks r t) p c)
rtpPayloadDemux payloadTable fallbackContent =
    mapC (timestamp %~ (MkTicks . fromIntegral . _rtpTimestamp)) .|
        awaitForever go
  where
    setFallbackContent = framePayload .~ fallbackContent
    go (MkStream (Next !frm)) =
        let pt = frm ^. framePayload . rtpPayloadType
            mHandler = Data.List.lookup pt payloadTable
            !frm' = fromMaybe setFallbackContent mHandler frm
        in
            yieldNextFrame frm'
    go (MkStream (Start !frmCtx)) =
        yieldStartFrameCtx frmCtx

type RtpPayloadHandler t c = Frame RtpSeqNum t RtpPayload
    -> Frame RtpSeqNum t c
