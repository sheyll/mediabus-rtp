-- | A high-level RTP receiver for G.711-ALAW.
module Data.MediaBus.Rtp.PcmAudioSource
  ( udpRtpPcmAudioSource16kHzS16SourceC, -- TODO Test
    pcmLittleEndianSigned16Bit16kHzRtpPayloadDecoderC, -- TODO Test
    coerceToPcmL16Payload
  )
where

import Conduit
import Control.Lens
import Control.Monad.Logger (MonadLogger)
import Data.MediaBus
import Data.MediaBus.Rtp.Packet
import Data.MediaBus.Rtp.Source
import Data.Streaming.Network (HostPreference)
import Data.Word

-- | Opend a UDP port and listen for PCM Signed 16bit 16kHz packets with the RTP payload type
udpRtpPcmAudioSource16kHzS16SourceC ::
  (MonadLogger m, MonadResource m) =>
  Int ->
  HostPreference ->
  Int ->
  RtpPayloadType ->
  ConduitT
    ()
    ( Stream
        RtpSsrc
        RtpSeqNum
        (Ticks (Hz 16000) Word64)
        ()
        (Audio (Hz 16000) Mono (Raw S16))
    )
    m
    ()
udpRtpPcmAudioSource16kHzS16SourceC !udpListenPort !udpListenIP !reorderBufferSize !pt =
       udpRtpSourceC udpListenPort udpListenIP
    .| rtpPayloadDispatcher [
          (pt, reorderFramesBySeqNumC reorderBufferSize .| pcmLittleEndianSigned16Bit16kHzRtpPayloadDecoderC)
          ]

pcmLittleEndianSigned16Bit16kHzRtpPayloadDecoderC :: Monad m => RtpPayloadConsumer () (Ticks (Hz 16000) Word64) (Audio (Hz 16000) Mono (Raw S16)) m
pcmLittleEndianSigned16Bit16kHzRtpPayloadDecoderC =
  mapFramesC' coerceToPcmL16Payload .| convertRtpTimestampC

-- | Coerce an 'RtpPayload' to a signed, little-endian 16 Bit buffer.
coerceToPcmL16Payload :: RtpPayloadHandler RtpTimestamp (Audio (Hz 16000) Mono (Raw S16))
coerceToPcmL16Payload =
  framePayload %~ (view (from rawPcmAudioBuffer) . mediaBufferFromByteString . _rtpPayload)

