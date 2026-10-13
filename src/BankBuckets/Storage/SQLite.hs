{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Storage.SQLite (newBucketRepo) where

import BankBuckets.Domain.Bucket (BucketRepo (..), bucketAmount, bucketId, bucketName)
import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Database.SQLite.Simple (Connection, Only (fromOnly), execute_, query)

newtype SQLiteRepo = SQLiteRepo Connection

newBucketRepo :: Connection -> IO SQLiteRepo
newBucketRepo conn = do
  execute_
    conn
    """
    CREATE TABLE IF NOT EXISTS buckets
      (id INTEGER PRIMARY KEY, name CHAR(100), amount FLOAT)
    """
  pure $ SQLiteRepo conn

instance BucketRepo SQLiteRepo where
  createBucket (SQLiteRepo conn) bucket = do
    res <-
      query
        conn
        "INSERT INTO buckets (name, amount) VALUES (?,?) RETURNING id"
        (bucket ^. bucketName, bucket ^. bucketAmount) ::
        IO [Only Int]
    pure $ bucket & bucketId .~ fromOnly (head res)

  updateBucket (SQLiteRepo conn) bucket = putStrLn $ "Updating bucket " ++ show bucket
  deleteBucket (SQLiteRepo conn) dbID = putStrLn $ "Deleting bucket " ++ show dbID
