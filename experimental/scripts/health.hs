{-
 Simple health script
 Example:
  ./.cabal-sandbox/bin/health 'redis://host:port' 'mysql://username:(password)@host:3306'
-}

{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances #-}

import qualified System.DevUtils.Redis.Helpers.Info as R
import qualified System.DevUtils.Redis.Helpers.Info.JSON as R
import qualified System.DevUtils.Redis.Helpers.CommandStats as R
import qualified System.DevUtils.Redis.Helpers.CommandStats.JSON as R
import qualified System.DevUtils.Redis.Tools as R
import qualified System.DevUtils.Base.Url.Redis as R
import qualified Database.Redis as R

import qualified System.DevUtils.MySQL.Helpers.ProcessList as My
import qualified System.DevUtils.MySQL.Simple as My
import qualified Database.MySQL.Simple as My

import System.DevUtils.Parser
import System.DevUtils.Base

import qualified Network.UberLog as Uber

import qualified Data.Aeson as Aeson

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as T


import System.Environment
import Control.Monad
import Data.Either
import GHC.Generics

usage :: String
usage = "usage: ./health <uberlog> <urls...>"

data Wrapper a = Wrapper {
 h :: String,
 d :: a
} deriving (Show, Read, Generic)

instance Aeson.FromJSON (Wrapper R.Info)
instance Aeson.ToJSON (Wrapper R.Info)

instance Aeson.FromJSON (Wrapper R.CommandStats)
instance Aeson.ToJSON (Wrapper R.CommandStats)

main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (url'ub:rest) -> do
   let urls = rights $ map (\url -> runCmd url) rest
   mapM_ print urls
   mapM_ (\url -> health url'ub url) urls
  _ -> error usage

health url'ub url = do
 case url of
  (UrlRedis redis) -> health'Redis url'ub redis
  (UrlMySQL mysql) -> health'MySQL url'ub mysql
  _ -> error "Invalid url"

health'Redis url'ub url = do
 r <- R.connect $ R.urlToConnectInfo url
 (Just inf) <- R.run'info r 
 print $ show inf
 ulog'inf <- Uber.new Uber.defaultUrl "ns" "cat" "info" []
 Uber.info (BS.concat $ BSL.toChunks (Aeson.encode (mk'Wrapper dest inf))) [] ulog'inf
 (Just cs) <- R.run'commandStats'List r
 ulog'cs <- Uber.new Uber.defaultUrl "ns" "cat" "commandstats" []
 Uber.info (BS.concat $ BSL.toChunks (Aeson.encode (mk'Wrapper dest cs))) [] ulog'cs
 return ()
 where
  dest = _dest (_con (R._ses url))

health'MySQL url'ub url = do
 return ()

mk'Wrapper url a = Wrapper {
 h = url,
 d = a
}
