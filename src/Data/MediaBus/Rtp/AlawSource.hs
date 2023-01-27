-- | A high-level RTP receiver for G.711-ALAW.
module Data.MediaBus.Rtp.AlawSource (
  udpRtpAlaw16kHzS16SourceC,
  coerceToAlawPayload,
  decodeAndResampleALaw,
)
where

import Conduit
import Control.Lens
import Control.Monad.Logger (MonadLogger)
import Data.MediaBus
import Data.MediaBus.Rtp.Packet
import Data.MediaBus.Rtp.Source
import Data.Proxy
import Data.Streaming.Network (HostPreference)
import Data.Word


-- | Opend a UDP port and listen for RTP- G711 Alaw packets
udpRtpAlaw16kHzS16SourceC ::
  (MonadLogger m, MonadResource m) =>
  Int ->
  HostPreference ->
  Int ->
  ConduitT
    ()
    (Stream RtpSsrc RtpSeqNum (Ticks (Hz 16000) Word64) () (Audio (Hz 16000) Mono (Raw S16)))
    m
    ()
udpRtpAlaw16kHzS16SourceC !udpListenPort !udpListenIP !reorderBufferSize =
       udpRtpSourceC udpListenPort udpListenIP
    .| rtpPayloadDispatcher [(8, decodeAndResampleALaw)]
    .| reorderFramesBySeqNumC reorderBufferSize

decodeAndResampleALaw ::
  Monad m =>
  ConduitT
    (RtpStream ())
    (Stream RtpSsrc RtpSeqNum (Ticks (Hz 16000) Word64) () (Audio (Hz 16000) Mono (Raw S16)))
    m
    ()
decodeAndResampleALaw =
     mapFramesC' coerceToAlawPayload
  .| convertRtpTimestampC
  .| annotateTypeCOut (Proxy :: Proxy (Stream RtpSsrc RtpSeqNum (Ticks (Hz 8000) Word32) () (Audio (Hz 8000) Mono (Raw S16)))) alawToS16
  .| resample8to16kHz' (0 :: Pcm Mono S16)
  .| convertTimestampC (Proxy :: Proxy '(Hz 8000, Word32)) (Proxy :: Proxy '(Hz 16000, Word64))

-- | Coerce an 'RtpPayload' to an 'ALaw' buffer.
coerceToAlawPayload :: RtpPayloadHandler RtpTimestamp (Audio (Hz 8000) Mono (Raw ALaw))
coerceToAlawPayload =
  framePayload %~ (view (from rawPcmAudioBuffer) . mediaBufferFromByteString . _rtpPayload)
