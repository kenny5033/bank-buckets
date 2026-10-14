{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Storage.SQLite (newBucketRepo) where

import BankBuckets.Domain.Bucket (Bucket (..), BucketRepo (..), bucketAmount, bucketId, bucketLastResolvedDateTime, bucketName, bucketPeriod, bucketPortion)
import Control.Lens ((.~), (^.))
import Data.Function ((&))
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX
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
      period BIGINT,
      last_resolved_date_time DATETIME
    )
    """
  pure $ SQLiteRepo conn

instance BucketRepo SQLiteRepo where
  createBucket (SQLiteRepo conn) bucket = do
    res <-
      query
        conn
        """
        INSERT INTO buckets (name, amount, portion, period, last_resolved_date_time)
        VALUES (?,?,?,?,?)
        RETURNING id
        """
        ( bucket ^. bucketName,
          bucket ^. bucketAmount,
          bucket ^. bucketPortion,
          round $ bucket ^. bucketPeriod :: Int,
          round $ utcTimeToPOSIXSeconds $ bucket ^. bucketLastResolvedDateTime :: Int
        ) ::
        IO [Only Int]
    pure $ bucket & bucketId .~ fromOnly (head res)

  readBucket (SQLiteRepo conn) dbId = do
    res <-
      query
        conn
        "SELECT id, name, amount, portion, period, last_resolved_date_time FROM buckets WHERE id = ?"
        (Only dbId) ::
        IO [(Int, String, Float, Float, Int, Int)]
    let (resId, resName, resAmount, resPortion, resPeriod, resLastResolvedDateTime) = head res
    pure $
      Bucket
        { _bucketId = resId,
          _bucketName = resName,
          _bucketAmount = resAmount,
          _bucketPortion = resPortion,
          _bucketPeriod = secondsToNominalDiffTime $ fromIntegral resPeriod,
          _bucketLastResolvedDateTime = posixSecondsToUTCTime $ fromIntegral resLastResolvedDateTime
        }

  updateBucket (SQLiteRepo conn) bucket = do
    execute
      conn
      "UPDATE buckets SET name = ?, amount = ?, portion = ?, period = ?, last_resolved_date_time WHERE id = ?"
      ( bucket ^. bucketName,
        bucket ^. bucketAmount,
        bucket ^. bucketId,
        bucket ^. bucketPortion,
        round $ bucket ^. bucketPeriod :: Int,
        round $ utcTimeToPOSIXSeconds $ bucket ^. bucketLastResolvedDateTime :: Int
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
        "SELECT id, name, amount, portion, period, last_resolved_date_time FROM buckets"
        () ::
        IO [(Int, String, Float, Float, Int, Int)]
    pure $ map recordToBucket res
    where
      recordToBucket (resId, resName, resAmount, resPortion, resPeriod, resLastResolvedDateTime) =
        Bucket
          { _bucketId = resId,
            _bucketName = resName,
            _bucketAmount = resAmount,
            _bucketPortion = resPortion,
            _bucketPeriod = secondsToNominalDiffTime $ fromIntegral resPeriod,
            _bucketLastResolvedDateTime = posixSecondsToUTCTime $ fromIntegral resLastResolvedDateTime
          }
