module UI (runUI) where

import Control.Concurrent.Chan             (Chan, dupChan, readChan)
import Control.Concurrent.STM.TQueue       (writeTQueue)
import Control.Monad                       (void)
import Control.Monad.Base                  (MonadBase(..))
import Control.Monad.IO.Class              (MonadIO)
import Control.Monad.Reader                (MonadReader(..))
import Control.Monad.STM                   (atomically)
import Control.Monad.Trans.Control         (MonadBaseControl)
import Control.Monad.Trans.Resource        (ResourceT, resourceForkIO)
import Data.Aeson                          (ToJSON, encode)
import Data.Binary.Builder                 (fromLazyByteString)
import Data.Function                       (fix)
import Data.Text.Lazy                      (Text)
import Foreign.C.Revolver                  (peers, streams)
import Data.Word                           (Word16)
import Network.Wai                         (StreamingBody)
import Network.Wai.EventSource.EventStream (ServerEvent(..), eventToBuilder)
import Web.Scotty.Trans                    (ScottyT, get, json, jsonData, post, scottyT, setHeader, stream)

import Config

runUI
  :: MonadBaseControl IO m
  => MonadReader Config m
  => MonadIO m
  => Word16
  -> ResourceT m ()
runUI port = do
  config <- runConfigT <$> ask
  void $ resourceForkIO $ scottyT n config app
  where n = fromIntegral port

app
  :: MonadBase IO m
  => MonadReader Config m
  => MonadIO m
  => ScottyT Text m ()
app = do
  get "/addresses" $ do
    Config {..} <- ask
    json $ cfgAddresses
  get "/peers" $ do
    Config {..} <- ask
    count <- liftBase $ peers cfgInterface
    json count
  get "/sse" $ do
    Config {..} <- ask
    messages <- liftBase $ dupChan cfgInbound
    setHeader "Content-Type" "text/event-stream"
    stream $ source messages
  get "/streams" $ do
    Config {..} <- ask
    count <- liftBase $ streams cfgInterface
    json count
  post "/chirp" $ do
    Config {..} <- ask
    message <- jsonData
    liftBase $ atomically $ writeTQueue cfgOutbound message

source
  :: ToJSON a
  => Chan a
  -> StreamingBody
source chan send flush = fix $ \ loop -> do
  builder <- fromLazyByteString . encode <$> readChan chan
  case eventToBuilder $ ServerEvent Nothing Nothing [builder] of
    Nothing -> pure ()
    Just chunk -> send chunk >> flush >> loop
