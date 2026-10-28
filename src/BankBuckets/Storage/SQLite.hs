{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Storage.SQLite (newBucketRepo) where

import BankBuckets.Domain.Bucket
import Control.Lens
import Data.Aeson
import Data.ByteString
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Database.SQLite.Simple

newtype SQLiteRepo = SQLiteRepo Connection

bucketToRowData :: Bucket -> Text
bucketToRowData = decodeUtf8 . toStrict . encode

rowDataToBucket :: (Int, Text) -> Maybe Bucket
rowDataToBucket (id', blob) = (bucketId .~ id') <$> loadedBucket
  where
    loadedBucket = (decode . fromStrict . encodeUtf8) blob

newBucketRepo :: Connection -> IO SQLiteRepo
newBucketRepo conn = do
  execute_
    conn
    """
    CREATE TABLE IF NOT EXISTS buckets
    (
      id INTEGER PRIMARY KEY,
      data TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(data))
    );
    """
  pure $ SQLiteRepo conn

instance BucketRepo SQLiteRepo where
  createBucket (SQLiteRepo conn) bucket = do
    res <-
      query
        conn
        "INSERT INTO buckets (data) VALUES (?) RETURNING id"
        (Only $ bucketToRowData bucket) ::
        IO [Only Int]
    case res of
      [] -> pure bucket
      x : _ -> pure $ bucket & bucketId .~ fromOnly x

  readBucket (SQLiteRepo conn) dbId = do
    res <-
      query
        conn
        "SELECT data FROM buckets WHERE id = ?"
        (Only dbId) ::
        IO [Only Text]
    case res of
      [] -> pure Nothing
      x : _ -> pure $ rowDataToBucket (dbId, fromOnly x)

  updateBucket (SQLiteRepo conn) bucket = do
    execute
      conn
      "UPDATE buckets SET data = ? WHERE id = ?"
      (Only $ bucketToRowData bucket)

  deleteBucket (SQLiteRepo conn) dbId = do
    execute
      conn
      "DELETE FROM buckets WHERE id = ?"
      (Only dbId)

  collectAllBuckets (SQLiteRepo conn) = do
    res <-
      query
        conn
        "SELECT id, data FROM buckets"
        () ::
        IO [(Int, Text)]
    pure $ mapMaybe rowDataToBucket res
