module Monitor (runMonitor, writeMetric) where

import Control.Concurrent.STM.TQueue (readTQueue, writeTQueue)
import Control.Monad                 (forever, void)
import Control.Monad.Base            (MonadBase(..))
import Control.Monad.Reader          (MonadReader(..))
import Control.Monad.STM             (atomically)
import Control.Monad.Trans.Control   (MonadBaseControl)
import Control.Monad.Trans.Resource  (ResourceT, resourceForkIO)

import Config
import Metric

runMonitor
  :: MonadBaseControl IO m
  => MonadReader Config m
  => ResourceT m ()
runMonitor = do
  Config {..} <- ask
  void $ resourceForkIO $ liftBase $ forever $ do
    Metric <- atomically $ readTQueue cfgMetrics
    -- TODO (derek): Send metrics to monitor server.
    pure ()

writeMetric
  :: MonadBase IO m
  => MonadReader Config m
  => Metric
  -> m ()
writeMetric metric = do
  Config {..} <- ask
  liftBase $ atomically $ writeTQueue cfgMetrics metric
