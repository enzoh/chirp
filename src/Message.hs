module Message (Message(..), verify) where

import Crypto.Hash.SHA256         (hash)
import Crypto.Secp256k1           (PubKey, Sig, msg, verifySig)
import Data.Aeson                 (FromJSON, ToJSON, Value(..), encode, toJSON)
import Data.Binary                (Binary)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Maybe                 (fromJust)
import Data.Text                  (Text)
import Data.Vector                (fromList)
import Data.Word                  (Word32)
import GHC.Generics               (Generic)

import Orphan ()

data Message =
  Message
  { msgData   :: {-# UNPACK #-} !Text
  , msgNonce  :: {-# UNPACK #-} !Word32
  , msgPubKey :: {-# UNPACK #-} !PubKey
  , msgSig    :: {-# UNPACK #-} !Sig
  } deriving (Generic, Show)

instance Binary Message
instance FromJSON Message
instance ToJSON Message

verify :: Message -> Bool
verify Message {..} = verifySig msgPubKey msgSig $ fromJust $ msg $ hash $ toStrict $ encode $ Array $ fromList [toJSON msgData, toJSON msgNonce, toJSON msgPubKey]
