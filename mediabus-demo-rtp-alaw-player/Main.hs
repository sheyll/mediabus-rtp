module Main where

import           System.Environment
import           Conduit
import           Data.MediaBus
import           Control.Monad.Logger
import           Data.MediaBus.Rtp

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
        [] -> mainASync
        -- some extra arguments indicate that no ringbuffer shall be
        -- put between sender and receiver.
        (_ : _) -> mainSync

mainASync :: IO ()
mainASync = runStdoutLoggingT $ runResourceT $
    withAsyncPolledSource 20
                          (rtpAlaw16kHzS16Source 10000 "127.0.0.1" 5)
                          (\(_, !src) -> runConduit (src .|
                                                         exitAfterC maxFrames .|
                                                         concealMissing .|
                                                         debugAudioPlaybackSink))

mainSync :: IO ()
mainSync = runStdoutLoggingT $ runConduitRes (rtpAlaw16kHzS16Source 10000 "127.0.0.1" 20 .|
                                              exitAfterC maxFrames .|
                                              debugAudioPlaybackSink)
