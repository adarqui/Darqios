{-
 Simple test script using "DevUtils" to find slow queries and/or too many queries on a mysql service
 Example:
  ./.cabal-sandbox/bin/mysql-alert-processlist 'mysql://username:(password)@host:3306' 50 1000
-}

import System.DevUtils.MySQL.Helpers.ProcessList
 (ProcessList(..), showFullProcessList)

import System.DevUtils.MySQL.Simple
 (urlToConnectInfo)

import System.DevUtils.Parser
 (runCmd)

import System.DevUtils.Parser.Cmd
 (Cmd(..))

import System.Environment
 (getArgs)

import Database.MySQL.Simple
 (ConnectInfo(..), connect)


usage :: String
usage = "./mysql-alert-processlist <mysql://url> <time_spent_threshold> <num_queries_threshold>"


queryMySQL :: String -> String -> String -> IO ()
queryMySQL url tsThreshold nqThreshold = queryMySQL' (runCmd url) (read tsThreshold :: Int) (read nqThreshold :: Int)


queryMySQL' :: Either String Cmd -> Int -> Int -> IO ()
queryMySQL' (Right (UrlMySQL my)) tsThreshold nqThreshold = do
 queryMySQL'' (urlToConnectInfo my) tsThreshold nqThreshold
queryMySQL' _ _ _ = do
 putStrLn "Invalid url"


queryMySQL'' :: ConnectInfo -> Int -> Int -> IO ()
queryMySQL'' ci tsThreshold nqThreshold = do
 conn <- connect ci
 vProcessList <- showFullProcessList conn
 let
  slowQueries = filter (\x -> (maybe False (\x1 -> x1 == "Query") (_command x)) && (maybe False (\x2 -> x2 > tsThreshold) (_time x))) vProcessList
  numQueries = length vProcessList
  d1 = case slowQueries of
   [] -> do return ()
   _ -> do
    print $ "Number of slow queries found: "++show (length slowQueries)
    print $ show slowQueries
  d2 = case (numQueries > nqThreshold) of
   False -> do return ()
   _ -> do
    print $ "Number of queries ("++show numQueries++") exceeds threshold of " ++ show nqThreshold
  in do
   d1
   d2


main :: IO ()
main = do
 argv <- getArgs
 case argv of
  (url:tsThreshold:nqThreshold:[]) -> queryMySQL url tsThreshold nqThreshold
  _ -> print usage
 return ()
