module Main where

import           System.Environment
import           Conduit
import           Data.MediaBus
import           Control.Monad.Logger
import           Data.MediaBus.Rtp
import System.Exit (exitWith, ExitCode (ExitFailure))

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
        ("async-tbqueue" : _) -> mainASyncTB
        ("async-framering" : _) -> mainASyncFR
        ("sync" : _) -> mainSync
        _ -> do
          myself <- getProgName
          putStrLn ""
          putStrLn "Usage:"
          putStrLn ""
          putStrLn (myself ++ " async-tbqueue | async-framering | sync")
          putStrLn ""
          exitWith (ExitFailure 1)

mainASyncTB :: IO ()
mainASyncTB = runStdoutLoggingT $ runResourceT $
    withAsyncPolledSource 20
                          (rtpAlaw16kHzS16Source 10000 "127.0.0.1" 5)
                          (\(_, !src) -> runConduit (src .|
                                                         exitAfterC maxFrames .|
                                                         concealMissing .|
                                                         debugAudioPlaybackSink))
mainASyncFR :: IO ()
mainASyncFR = runStdoutLoggingT $ runResourceT $
    withConcurrentSource 20
                          (rtpAlaw16kHzS16Source 10000 "127.0.0.1" 5 .| assumeSynchronizedC)
                          (\(_, !src) -> runConduit (src .|
                                                         exitAfterC maxFrames .|
                                                         setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @RtpSeqNum  @Integer .|
                                                         concealMissing .|
                                                         debugAudioPlaybackSink))

mainSync :: IO ()
mainSync = runStdoutLoggingT $ runConduitRes (rtpAlaw16kHzS16Source 10000 "127.0.0.1" 20 .|
                                              exitAfterC maxFrames .|
                                              debugAudioPlaybackSink)
