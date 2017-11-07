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
import Log

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
    writeLog Debug "GET /addresses"
    Config {..} <- ask
    json $ cfgAddresses

  get "/messages" $ do
    writeLog Debug "GET /messages"
    Config {..} <- ask
    messages <- liftBase $ dupChan cfgInbound
    setHeader "Content-Type" "text/event-stream"
    stream $ sse messages

  get "/peers" $ do
    writeLog Debug "GET /peers"
    Config {..} <- ask
    count <- liftBase $ peers cfgInterface
    json $ pred count

  get "/streams" $ do
    writeLog Debug "GET /streams"
    Config {..} <- ask
    count <- liftBase $ streams cfgInterface
    json count

  post "/message" $ do
    writeLog Debug "POST /message"
    Config {..} <- ask
    message <- jsonData
    liftBase $ atomically $ writeTQueue cfgOutbound message

sse
  :: ToJSON a
  => Chan a
  -> StreamingBody
sse chan send flush = fix $ \ loop -> do
  builder <- fromLazyByteString . encode <$> readChan chan
  case eventToBuilder $ ServerEvent Nothing Nothing [builder] of
    Nothing -> pure ()
    Just chunk -> send chunk >> flush >> loop
