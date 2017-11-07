module Config (Config(..), runConfigT) where

import Control.Concurrent.Chan       (Chan)
import Control.Concurrent.STM.TQueue (TQueue)
import Control.Monad.Reader          (ReaderT(..))
import Foreign.C.Revolver            (RevolverId)
import System.Logger                 (Level)

import Message
import Metric

data Config =
  Config
  { cfgAddresses :: [String]
  , cfgInbound   :: Chan Message
  , cfgInterface :: RevolverId
  , cfgLogs      :: TQueue (Level, String)
  , cfgMetrics   :: TQueue Metric
  , cfgOutbound  :: TQueue Message
  }

runConfigT
  :: Monad m
  => Config
  -> ReaderT Config m a
  -> m a
runConfigT = flip runReaderT
