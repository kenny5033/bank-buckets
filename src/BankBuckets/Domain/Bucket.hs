{-# LANGUAGE TemplateHaskell #-}

module BankBuckets.Domain.Bucket
  ( bucketId,
    bucketName,
    bucketAmount,
    bucketPortion,
    bucketPeriod,
    BucketRepo (..),
    displayBucket,
    changeBucketAmount,
    newBucket,
    Bucket (..),
  )
where

import Control.Lens
import Data.Time
import Text.Printf (printf)

data Bucket = Bucket
  { _bucketId :: Int,
    _bucketName :: String,
    _bucketAmount :: Float,
    _bucketPortion :: Float,
    _bucketPeriod :: NominalDiffTime
  }
  deriving (Show)

$(makeLenses ''Bucket)

newBucket :: String -> Bucket
newBucket name = Bucket {_bucketId = 0, _bucketName = name, _bucketAmount = 0, _bucketPortion = 0, _bucketPeriod = secondsToNominalDiffTime 0}

displayBucket :: Bucket -> String
displayBucket (Bucket id' name amount portion period) = printf "(%d) %s | %f <- %f / %s" id' name amount portion (show period)

changeBucketAmount :: Bucket -> Float -> Bucket
changeBucketAmount b x = b & bucketAmount %~ (+ x)

class BucketRepo repo where
  createBucket :: repo -> Bucket -> IO Bucket
  readBucket :: repo -> Int -> IO Bucket
  updateBucket :: repo -> Bucket -> IO ()
  deleteBucket :: repo -> Int -> IO ()

  collectAllBuckets :: repo -> IO [Bucket]
