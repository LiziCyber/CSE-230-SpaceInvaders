{-# LANGUAGE OverloadedStrings #-}
module Leaderboard where

import Database.SQLite.Simple
import Data.Time (UTCTime)

data LeaderboardRecord = LeaderboardRecord
  {
    time    :: UTCTime
  , score'  :: Int
  , name    :: String
  } deriving (Show)

data Leaderboard = Leaderboard 
  {
    records   :: [LeaderboardRecord]
  , startIdx  :: Int
  } deriving (Show)

recordsPerPage :: Int
recordsPerPage = 5

instance FromRow LeaderboardRecord where
    fromRow = LeaderboardRecord <$> field <*> field <*> field

withConn :: String -> (Connection -> IO a) -> IO a
withConn dbName action = do
    conn <- open dbName
    result <- action conn
    close conn
    return result

createTable :: Connection -> IO ()
createTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS leaderboard (timestamp TIMESTAMP PRIMARY KEY, score INTEGER, name TEXT)"

insertRecord :: LeaderboardRecord -> IO ()
insertRecord record = withConn "leaderboard.db" $ \conn -> do
  createTable conn
  execute conn "INSERT INTO leaderboard (timestamp, score, name) VALUES (?, ?, ?)" (time record, score' record, name record)

getRecords :: IO [LeaderboardRecord]
getRecords = withConn "leaderboard.db" $ \conn -> do
  createTable conn
  query_ conn "SELECT * from leaderboard ORDER BY score DESC"

