module Seed (partition, select) where

import Control.Exception     (try)
import Control.Monad         (replicateM)
import Crypto.Hash.SHA256    (hash)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Foldable         (fold)
import Data.Maybe            (fromJust)
import Data.Monoid           (mappend)
import Data.RFC1751          (keyToMnemonic, mnemonicToKey)
import System.Directory      (createDirectoryIfMissing)
import System.Entropy        (getEntropy)
import System.FilePath       ((</>))
import System.IO.Error       (isDoesNotExistError)

select :: FilePath -> FilePath -> IO ByteString
select dir file = do
  let path = dir </> file
  side <- try $ readFile path
  case side of
    Left err | isDoesNotExistError err -> do
      keys <- replicateM 2 $ getEntropy 16
      createDirectoryIfMissing True dir
      let seeds = fromJust $ sequence $ map keyToMnemonic keys
      writeFile path $ unwords seeds ++ "\n"
      pure $ fold keys
    Left err -> fail $ show err
    Right contents -> do
      let seeds = group $ words contents
      case sequence $ map mnemonicToKey seeds of
        Nothing -> fail "invalid seed"
        Just keys -> pure $ fold keys
  where
  group xs
    | null xs = []
    | otherwise = let (a, b) = splitAt 12 xs in unwords a : group b

partition :: ByteString -> Int -> [ByteString]
partition seed n = flip map [1..n] $ hash . mappend seed . pack . show
