module Main where

import Conduit (runConduit, runConduitRes, runResourceT, (.|))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.MediaBus
  ( Hz,
    assumeSynchronizedC,
    concealMissing,
    debugAudioPlaybackSink,
    exitAfterC,
    setSequenceNumberAndTimestampC,
    withConcurrentSource,
  )
import Data.MediaBus.Conduit.Async (withAsyncPolledSource)
import Data.MediaBus.Rtp (RtpSeqNum, rtpAlaw16kHzS16Source)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Numeric.Natural

{- Send test data using the script in ./rtp-sender/send.sh.
 -
 - DONT FORGET TO COMPILE WITH '-threaded'!!!
-}
maxFrames :: Int
maxFrames = 15000

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("async-tbqueue" : sizeStr : _) -> mainASyncTB (read sizeStr)
    ("async-tbqueue" : _) -> mainASyncTB 10
    ("async-framering" : sizeStr : _) -> mainASyncFR (read sizeStr)
    ("async-framering" : _) -> mainASyncFR 10
    ("sync" : _) -> mainSync
    _ -> do
      myself <- getProgName
      putStrLn ""
      putStrLn "Usage:"
      putStrLn ""
      putStrLn (myself ++ " async-tbqueue [queue size] | async-framering [ring size] | sync")
      putStrLn ""
      exitWith (ExitFailure 1)

mainASyncTB :: Natural -> IO ()
mainASyncTB n =
  runStdoutLoggingT $
    runResourceT $
      withAsyncPolledSource
        n
        (rtpAlaw16kHzS16Source 10000 "127.0.0.1" 5)
        ( \(_, !src) ->
            runConduit
              ( src
                  .| exitAfterC maxFrames
                  .| concealMissing
                  .| debugAudioPlaybackSink
              )
        )

mainASyncFR :: Natural -> IO ()
mainASyncFR n =
  runStdoutLoggingT $
    runResourceT $
      withConcurrentSource
        n
        (rtpAlaw16kHzS16Source 10000 "127.0.0.1" 5 .| assumeSynchronizedC)
        ( \(_, !src) ->
            runConduit
              ( src
                  .| exitAfterC maxFrames
                  .| setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @RtpSeqNum @Integer
                  .| concealMissing
                  .| debugAudioPlaybackSink
              )
        )

mainSync :: IO ()
mainSync =
  runStdoutLoggingT $
    runConduitRes
      ( rtpAlaw16kHzS16Source 10000 "127.0.0.1" 20
          .| exitAfterC maxFrames
          .| debugAudioPlaybackSink
      )
