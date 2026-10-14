{-# LANGUAGE TemplateHaskell #-}

module BankBuckets.Domain.Bucket
  ( bucketId,
    bucketName,
    bucketAmount,
    bucketPortion,
    bucketPeriod,
    bucketLastResolvedDateTime,
    BucketRepo (..),
    Bucket (..),
  )
where

import Control.Lens
import Data.Time

data Bucket = Bucket
  { _bucketId :: Int,
    _bucketName :: String,
    _bucketAmount :: Float,
    _bucketPortion :: Float,
    _bucketPeriod :: NominalDiffTime,
    _bucketLastResolvedDateTime :: UTCTime
  }
  deriving (Show)

$(makeLenses ''Bucket)

class BucketRepo repo where
  createBucket :: repo -> Bucket -> IO Bucket
  readBucket :: repo -> Int -> IO Bucket
  updateBucket :: repo -> Bucket -> IO ()
  deleteBucket :: repo -> Int -> IO ()

  collectAllBuckets :: repo -> IO [Bucket]
