{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Storage.SQLite (newBucketRepo) where

import BankBuckets.Domain.Bucket (Bucket (..), BucketRepo (..), bucketAmount, bucketId, bucketName, bucketPeriod, bucketPortion)
import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Data.Time (secondsToNominalDiffTime)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), execute, execute_, query)

newtype SQLiteRepo = SQLiteRepo Connection

newBucketRepo :: Connection -> IO SQLiteRepo
newBucketRepo conn = do
  execute_
    conn
    """
    CREATE TABLE IF NOT EXISTS buckets
    (
      id INTEGER PRIMARY KEY,
      name CHAR(100),
      amount FLOAT,
      portion FLOAT,
      period BIGINT
    )
    """
  pure $ SQLiteRepo conn

instance BucketRepo SQLiteRepo where
  createBucket (SQLiteRepo conn) bucket = do
    res <-
      query
        conn
        "INSERT INTO buckets (name, amount, portion, period) VALUES (?,?,?,?) RETURNING id"
        ( bucket ^. bucketName,
          bucket ^. bucketAmount,
          bucket ^. bucketPortion,
          round $ bucket ^. bucketPeriod :: Int
        ) ::
        IO [Only Int]
    pure $ bucket & bucketId .~ fromOnly (head res)

  readBucket (SQLiteRepo conn) dbId = do
    res <-
      query
        conn
        "SELECT id, name, amount, portion, period FROM buckets WHERE id = ?"
        (Only dbId) ::
        IO [(Int, String, Float, Float, Int)]
    let (resId, resName, resAmount, resPortion, resPeriod) = head res
    pure $
      Bucket
        { _bucketId = resId,
          _bucketName = resName,
          _bucketAmount = resAmount,
          _bucketPortion = resPortion,
          _bucketPeriod = secondsToNominalDiffTime $ fromIntegral resPeriod
        }

  updateBucket (SQLiteRepo conn) bucket = do
    execute
      conn
      "UPDATE buckets SET name = ?, amount = ?, portion = ?, period = ? WHERE id = ?"
      ( bucket ^. bucketName,
        bucket ^. bucketAmount,
        bucket ^. bucketId,
        bucket ^. bucketPortion,
        round $ bucket ^. bucketPeriod :: Int
      )

  deleteBucket (SQLiteRepo conn) dbId = do
    execute
      conn
      "DELETE FROM buckets WHERE id = ?"
      (Only dbId)

  collectAllBuckets (SQLiteRepo conn) = do
    res <-
      query
        conn
        "SELECT id, name, amount, portion, period FROM buckets"
        () ::
        IO [(Int, String, Float, Float, Int)]
    pure $ map recordToBucket res
    where
      recordToBucket (resId, resName, resAmount, resPortion, resPeriod) =
        Bucket
          { _bucketId = resId,
            _bucketName = resName,
            _bucketAmount = resAmount,
            _bucketPortion = resPortion,
            _bucketPeriod = secondsToNominalDiffTime $ fromIntegral resPeriod
          }
