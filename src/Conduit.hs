module Conduit (runConduit) where

import Control.Concurrent.STM.TQueue (readTQueue)
import Control.Monad                 ((<=<), forever)
import Control.Monad.Base            (MonadBase(..))
import Control.Monad.Reader          (MonadReader(..))
import Control.Monad.STM             (atomically)
import Control.Monad.Trans.Class     (lift)
import Control.Monad.Trans.Control   (MonadBaseControl)
import Control.Monad.Trans.Resource  (MonadResource)
import Data.Binary                   (decodeOrFail, encode)
import Data.ByteString.Lazy          (fromStrict, toStrict)
import Data.Conduit                  (ConduitM, Sink, Source, ($=), ($$), awaitForever, yield)
import Data.Conduit.TMChan           (mergeSources)
import Foreign.C.Revolver            (receive, send)

import Config
import Log
import Message

runConduit
  :: MonadBase IO m
  => MonadBaseControl IO m
  => MonadReader Config m
  => MonadResource m
  => ConduitM Message Message m ()
  -> m ()
runConduit conduit =
  messageSource $= conduit $$ messageSink

messageSource
  :: MonadBase IO m
  => MonadBaseControl IO m
  => MonadReader Config m
  => MonadResource m
  => Source m Message
messageSource =
  -- TODO (enzo): Measure appropriate queue size.
  id <=< lift $ mergeSources [messageSourceP2P, messageSourceUI] 1000

messageSourceP2P
  :: MonadBase IO m
  => MonadReader Config m
  => Source m Message
messageSourceP2P = do
  Config {..} <- ask
  forever $ do
    bytes <- liftBase $ receive cfgInterface
    case decodeOrFail $ fromStrict bytes of
      Left (_, _, err) -> do
        writeLog Debug err
        -- TODO (enzo): Disconnect from sender.
      Right (_, _, message) -> do
        if verify message
        then yield message
        else do
          writeLog Debug "invalid signature"
          -- TODO (enzo): Disconnect from sender.

messageSourceUI
  :: MonadBase IO m
  => MonadReader Config m
  => Source m Message
messageSourceUI = do
  Config {..} <- ask
  forever $ do
    message <- liftBase $ atomically $ readTQueue cfgOutbound
    if verify message
    then yield message
    else writeLog Debug "invalid signature"

messageSink
  :: MonadBase IO m
  => MonadReader Config m
  => Sink Message m ()
messageSink = do
  Config {..} <- ask
  awaitForever $ liftBase . send cfgInterface . toStrict . encode
