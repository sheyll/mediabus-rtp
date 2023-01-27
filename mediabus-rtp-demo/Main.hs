{-# LANGUAGE NumericUnderscores #-}
module Main where

import Conduit (runConduit, runConduitRes, runResourceT, (.|), MonadResource, ConduitT)
import Control.Monad.Logger (runStdoutLoggingT, MonadLogger)
import Data.MediaBus
    ( Stream,
      Ticks,
      debugAudioPlaybackSink,
      concealMissing,
      withConcurrentSource,
      reorderFramesBySeqNumC,
      forgetStaticSegmentationC,
      staticSegmentC',
      assumeSynchronizedC,
      setSequenceNumberAndTimestampC,
      exitAfterC,
      type (:/),
      Hz,
      Audio,
      Raw,
      Mono,
      S16 )
import Data.MediaBus.Conduit.Async (withAsyncPolledSource)
import Data.Proxy ( Proxy(Proxy) )
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Numeric.Natural ( Natural )
import Data.Conduit.Network.UDP ( HostPreference )
import Data.MediaBus.Rtp.Packet ( RtpSeqNum, RtpSsrc, RtpPayloadType )
import Data.Word ( Word64 )
import Data.MediaBus.Rtp.Source
    ( rtpPayloadDispatcher, udpRtpSourceC )
import Data.MediaBus.Rtp.AlawSource
    ( udpRtpAlaw16kHzS16SourceC, decodeAndResampleALaw )
import Data.MediaBus.Rtp.PcmAudioSource
    ( udpRtpPcmAudioSource16kHzS16SourceC, pcmLittleEndianSigned16Bit16kHzRtpPayloadDecoderC )

{- Send test data using the script in ./rtp-sender/send.sh.
 -
 - DONT FORGET TO COMPILE WITH '-threaded'!!!
-}
maxFrames :: Int
maxFrames = 15_000

reorderBufferSize :: Int
reorderBufferSize = 5

listenHost :: HostPreference
listenHost = "127.0.0.1"

listenPort :: Int
listenPort = 10_000

pcm16PayloadType :: RtpPayloadType
pcm16PayloadType = 100

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("g711":rest) -> main2 g711Receiver rest
    ("pcm16":rest) -> main2 pcm16Receiver rest
    _ -> main2 g711AndPcm16Receiver args

main2 :: Receiver -> [String] -> IO ()
main2 receiver args =
  case args of
    ("async-tbqueue" : sizeStr : _) -> mainASyncTB receiver (read sizeStr)
    ("async-tbqueue" : _) -> mainASyncTB receiver 10
    ("async-framering" : sizeStr : _) -> mainASyncFR receiver (read sizeStr)
    ("async-framering" : _) -> mainASyncFR receiver 10
    ("sync" : _) -> mainSync receiver
    _ -> do
      myself <- getProgName
      putStrLn ""
      putStrLn "Usage:"
      putStrLn ""
      putStrLn (myself ++ " [g711|pcm16] async-tbqueue [queue size] | async-framering [ring size] | sync")
      putStrLn ""
      exitWith (ExitFailure 1)

type EnterpriseM m = (MonadLogger m, MonadResource m)
type ReceiverC m = ConduitT () (Stream RtpSsrc RtpSeqNum (Ticks (Hz 16000) Word64) () (Audio (Hz 16000) Mono (Raw S16))) m ()
type Receiver = (forall m. EnterpriseM m => ReceiverC m)

g711Receiver :: Receiver
g711Receiver = udpRtpAlaw16kHzS16SourceC listenPort listenHost reorderBufferSize

pcm16Receiver :: EnterpriseM m => ReceiverC m
pcm16Receiver = udpRtpPcmAudioSource16kHzS16SourceC listenPort listenHost reorderBufferSize pcm16PayloadType

g711AndPcm16Receiver :: Receiver
g711AndPcm16Receiver =
       udpRtpSourceC listenPort listenHost
    .| rtpPayloadDispatcher [
          (8,   reorderFramesBySeqNumC reorderBufferSize .| decodeAndResampleALaw),
          (100, reorderFramesBySeqNumC reorderBufferSize .| pcmLittleEndianSigned16Bit16kHzRtpPayloadDecoderC)
          ]


mainASyncTB :: Receiver -> Natural -> IO ()
mainASyncTB receiver n =
  runStdoutLoggingT $
    runResourceT $
      withAsyncPolledSource
        n
        ( receiver
        .| staticSegmentC' (Proxy @(320 :/ Hz 16000))
        )
        ( \(_, !src) ->
            runConduit
              ( src
                  .| exitAfterC maxFrames
                  .| concealMissing
                  .| forgetStaticSegmentationC
                  .| debugAudioPlaybackSink
              )
        )

mainASyncFR :: Receiver -> Natural -> IO ()
mainASyncFR receiver n =
  runStdoutLoggingT $
    runResourceT $
      withConcurrentSource
        n
        (receiver
          .| staticSegmentC' (Proxy @(400 :/ Hz 16000))
          .| assumeSynchronizedC
          )
        ( \(_, !src) ->
            runConduit
              ( src
                  .| exitAfterC maxFrames
                  .| setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @RtpSeqNum @Integer
                  .| concealMissing
                  .| forgetStaticSegmentationC
                  .| debugAudioPlaybackSink
              )
        )

mainSync :: Receiver -> IO ()
mainSync receiver =
  runStdoutLoggingT $
    runConduitRes
      ( receiver
          .| exitAfterC maxFrames
          .| debugAudioPlaybackSink
      )


