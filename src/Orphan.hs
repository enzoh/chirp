module Orphan () where

import Control.Monad             (mzero)
import Control.Monad.Except      (mapExceptT)
import Control.Monad.Reader      (MonadReader(..), mapReaderT)
import Control.Monad.State.Lazy  (StateT(..), evalStateT)
import Control.Monad.Trans.Class (lift)
import Crypto.Secp256k1          (PubKey, Sig, exportPubKey, exportSig, importPubKey, importSig)
import Data.Aeson                (FromJSON(..), ToJSON(..), Value(..))
import Data.Binary               (Binary(..))
import Data.ByteString.Base16    (decode, encode)
import Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import Web.Scotty.Internal.Types (ActionT(..), ScottyError)

instance Binary PubKey where
  put = put . exportPubKey True
  get = maybe "" id . importPubKey <$> get

instance Binary Sig where
  put = put . exportSig
  get = maybe "" id . importSig <$> get

instance FromJSON PubKey where
  parseJSON = \ case
    String text -> maybe mzero pure $ importPubKey $ fst $ decode $ encodeUtf8 text
    _ -> mzero

instance FromJSON Sig where
  parseJSON = \ case
    String text -> maybe mzero pure $ importSig $ fst $ decode $ encodeUtf8 text
    _ -> mzero

instance (ScottyError e, MonadReader r m) => MonadReader r (ActionT e m) where
  ask = lift ask
  local f = mapActionT $ local f

instance ToJSON PubKey where
  toJSON = String . decodeUtf8 . encode . exportPubKey True

instance ToJSON Sig where
  toJSON = String . decodeUtf8 . encode . exportSig

mapActionT :: (Monad η, Monad µ, ScottyError e) => (forall a . η a -> µ a) -> ActionT e η b -> ActionT e µ b
mapActionT f action = ActionT $ flip mapExceptT except $ \ reader -> flip mapReaderT reader $ \ state -> StateT $ \ x -> (,x) <$> f (evalStateT state x) where except = runAM action
