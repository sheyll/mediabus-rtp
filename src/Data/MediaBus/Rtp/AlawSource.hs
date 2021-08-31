-- | A high-level RTP receiver for G.711-ALAW.
module Data.MediaBus.Rtp.AlawSource
  ( rtpAlaw16kHzS16Source
  , alawPayloadHandler
  ) where

import Conduit
import qualified Data.ByteString as B
import Data.MediaBus
import Data.MediaBus.Rtp.Packet
import Data.MediaBus.Rtp.Source
import Data.Proxy
import Data.Streaming.Network (HostPreference)
import Data.Word
import Network.Socket (SockAddr)
import Control.Lens
import Data.Coerce
import Control.Monad.Logger

-- | Opend a UDP port and listen for RTP- G711 Alaw packets
rtpAlaw16kHzS16Source
  :: (MonadLogger m, MonadResource m)
  => Int
  -> HostPreference
  -> Int
  -> ConduitT
        ()
        (Stream
          RtpSsrc RtpSeqNum (Ticks (Hz 16000) Word64)
          () (Audio (Hz 16000) Mono (Raw S16))) m ()
rtpAlaw16kHzS16Source !udpListenPort !udpListenIP !reorderBufferSize =
  annotateTypeSource
    (Proxy :: Proxy (Stream (SourceId (Maybe SockAddr)) RtpSeqNum (ClockTimeDiff UtcClock) () B.ByteString))
    (udpDatagramSource useUtcClock udpListenPort udpListenIP) .|
  rtpSource .|
  rtpPayloadDemux [(8, alawPayloadHandler)] mempty .|
  annotateTypeCOut
    (Proxy :: Proxy (Stream RtpSsrc RtpSeqNum (Ticks (Hz 8000) Word32) () (Audio (Hz 8000) Mono (Raw S16))))
    alawToS16 .|
  resample8to16kHz' (0 :: Pcm Mono S16) .|
  convertTimestampC
    (Proxy :: Proxy '( Hz 8000, Word32))
    (Proxy :: Proxy '( Hz 16000, Word64)) .|
  reorderFramesBySeqNumC reorderBufferSize

-- | Coerce an 'RtpPayload' to an 'ALaw' buffer.
alawPayloadHandler :: RtpPayloadHandler t (Audio (Hz 8000) Mono (Raw ALaw))
alawPayloadHandler =
  framePayload %~ (view (from pcmMediaBuffer) . coerce . _rtpPayload)
