-- | A high-level RTP receiver for G.711-ALAW.
module Data.MediaBus.Rtp.AlawSource (
  udpRtpAlaw16kHzS16SourceC,
  coerceToAlawPayload,
  decodeAndResampleALaw,
  AlawDecoderState(),
  withAlawDecoderState,
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
import Data.IORef


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
  withAlawDecoderState $ \st ->
       udpRtpSourceC udpListenPort udpListenIP
    .| rtpPayloadDispatcher [(8, decodeAndResampleALaw st)]
    .| reorderFramesBySeqNumC reorderBufferSize

-- | The state type used by the A-Law decoder. See 'withAlawDecoderState'.
newtype AlawDecoderState = MkAlawDecoderState (IORef (Pcm Mono S16))

-- | Create the state used by the A-Law decoder. See 'decodeAndResampleALaw'.
withAlawDecoderState :: MonadIO m => (AlawDecoderState -> ConduitT i o m r) -> ConduitT i o m r
withAlawDecoderState k = do
  liftIO (newIORef (0 :: Pcm Mono S16)) >>= k . MkAlawDecoderState


-- | Return an ALaw decoder. Requires external state, see 'withAlawDecoderState'.
decodeAndResampleALaw ::
  (Monad m, MonadIO m) =>
  AlawDecoderState ->
  ConduitT
    (RtpStream ())
    (Stream RtpSsrc RtpSeqNum (Ticks (Hz 16000) Word64) () (Audio (Hz 16000) Mono (Raw S16)))
    m
    ()
decodeAndResampleALaw (MkAlawDecoderState !resampleRef) =
  mapFramesC' coerceToAlawPayload
   .| convertRtpTimestampC
   .| annotateTypeCOut (Proxy :: Proxy (Stream RtpSsrc RtpSeqNum (Ticks (Hz 8000) Word32) () (Audio (Hz 8000) Mono (Raw S16)))) alawToS16
   .| resample8to16kHz' resampleRef
   .| convertTimestampC (Proxy :: Proxy '(Hz 8000, Word32)) (Proxy :: Proxy '(Hz 16000, Word64))

-- | Coerce an 'RtpPayload' to an 'ALaw' buffer.
coerceToAlawPayload :: RtpPayloadHandler RtpTimestamp (Audio (Hz 8000) Mono (Raw ALaw))
coerceToAlawPayload =
  framePayload %~ (view (from rawPcmAudioBuffer) . mediaBufferFromByteString . _rtpPayload)
