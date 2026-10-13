{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Storage.SQLite (newBucketRepo) where

import BankBuckets.Domain.Bucket (Bucket (..), BucketRepo (..), bucketAmount, bucketId, bucketName)
import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), execute, execute_, query)

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

  readBucket (SQLiteRepo conn) dbId = do
    res <-
      query
        conn
        "SELECT id, name, amount FROM buckets WHERE id = ?"
        (Only dbId) ::
        IO [(Int, String, Float)]
    let (resId, resName, resAmount) = head res
    pure $ Bucket {_bucketId = resId, _bucketName = resName, _bucketAmount = resAmount}

  updateBucket (SQLiteRepo conn) bucket = do
    execute
      conn
      "UPDATE buckets SET name = ?, amount = ? WHERE id = ?"
      (bucket ^. bucketName, bucket ^. bucketAmount, bucket ^. bucketId)

  deleteBucket (SQLiteRepo conn) dbId = do
    execute
      conn
      "DELETE FROM buckets WHERE id = ?"
      (Only dbId)
