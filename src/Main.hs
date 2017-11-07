module Main where

import Control.Concurrent.Chan       (newChan, writeChan)
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Exception             (bracket)
import Control.Monad.Base            (MonadBase(..))
import Control.Monad.Reader          (MonadReader(..))
import Control.Monad.Trans.Resource  (runResourceT)
import Data.ByteString.Base16        (encode)
import Data.ByteString.Char8         (ByteString, unpack)
import Data.Conduit                  (ConduitM, awaitForever, yield)
import Data.Default.Class            (Default(..))
import Data.Word                     (Word16)
import Foreign.C.Revolver hiding     (Config(Config))
import System.Console.CmdArgs        (Data, cmdArgs)
import System.Directory              (getHomeDirectory)
import System.FilePath               ((</>))

import Conduit
import Config
import Log
import Message
import Seed
import UI

data Options =
  Options
  { p2p_port   :: Word16
  , seed_nodes :: String
  , ui_port    :: Word16
  } deriving Data

instance Default Options where
  def = Options 4000 "" 4001

main :: IO ()
main = do
  Options {..} <- cmdArgs def
  home <- getHomeDirectory
  let dir = home </> ".chirp"
  seed <- select dir "default"
  let network = bootstrap p2p_port seed seed_nodes
  bracket network shutdown $ \ interface -> do
    addrs <- addresses interface
    id <- identity interface
    logs <- newTQueueIO
    inbound <- newChan
    outbound <- newTQueueIO
    let addresses = flip map addrs $ \ addr -> addr </> "ipfs" </> id
    let config = Config addresses inbound interface logs outbound
    runConfigT config $ runResourceT $ do
      runLogger
      runUI ui_port
      runConduit conduit

bootstrap
  :: Word16
  -> ByteString
  -> String
  -> IO RevolverId
bootstrap port seed nodes = new def
  { cfgLogLevel = "WARNING"
  , cfgNetwork = "chirp"
  , cfgPort = port
  , cfgRandomSeed = unpack $ encode seed
  , cfgSeedNodes = split nodes
  , cfgVersion = "0.0.0"
  }

conduit
  :: MonadBase IO m
  => MonadReader Config m
  => ConduitM Message Message m ()
conduit = do
  Config {..} <- ask
  awaitForever $ \ message -> do
    liftBase $ writeChan cfgInbound message
    yield message

split :: String -> [String]
split xs
  | null xs = []
  | otherwise = let (a, b) = break (==',') xs in (:) a $ split $ drop 1 b
