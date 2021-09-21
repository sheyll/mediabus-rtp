{-# OPTIONS -Wno-unused-top-binds #-}

module Data.MediaBus.Rtp
  ( module X,
    isRtpLogSource,
  )
where

import Data.MediaBus.Rtp.AlawSource as X
import Data.MediaBus.Rtp.InternalLogging (myLogSource)
import Data.MediaBus.Rtp.Packet as X
import Data.MediaBus.Rtp.Source as X
import Control.Monad.Logger ( LogSource )

-- | A predicate for log messages from this library, based on the 'LogSource'
isRtpLogSource :: LogSource -> Bool
isRtpLogSource = (==) myLogSource