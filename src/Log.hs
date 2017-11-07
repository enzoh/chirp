module Log (Level(..), runLogger, writeLog) where

import Control.Concurrent.STM.TQueue (readTQueue, writeTQueue)
import Control.Monad                 (forever, void)
import Control.Monad.Base            (MonadBase(..))
import Control.Monad.Reader          (MonadReader(..))
import Control.Monad.STM             (atomically)
import Control.Monad.Trans.Control   (MonadBaseControl)
import Control.Monad.Trans.Resource  (ResourceT, resourceForkIO)
import System.Logger as Log          (Level(..), defSettings, log, new)
import System.Logger.Message         (msg)

import Config

runLogger
  :: MonadBaseControl IO m
  => MonadReader Config m
  => ResourceT m ()
runLogger = do
  Config {..} <- ask
  void $ resourceForkIO $ liftBase $ do
    logger <- new defSettings
    forever $ do
      (,) level message <- atomically $ readTQueue cfgLogs
      Log.log logger level $ msg message

writeLog
  :: MonadBase IO m
  => MonadReader Config m
  => Level
  -> String
  -> m ()
writeLog level message = do
  Config {..} <- ask
  liftBase $ atomically $ writeTQueue cfgLogs (level, message)
