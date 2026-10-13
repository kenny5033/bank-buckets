{-# LANGUAGE TemplateHaskell #-}

module BankBuckets.Domain.Bucket
  ( bucketId,
    bucketName,
    bucketAmount,
    BucketRepo (..),
    displayBucket,
    changeBucketAmount,
    newBucket,
  )
where

import BankBuckets.Domain.Common (DomainObject (..))
import Control.Lens
import Text.Printf (printf)

data Bucket = Bucket
  { _bucketId :: Int,
    _bucketName :: String,
    _bucketAmount :: Float
  }
  deriving (Show)

$(makeLenses ''Bucket)

newBucket :: String -> Bucket
newBucket name = Bucket {_bucketId = 0, _bucketName = name, _bucketAmount = 0}

displayBucket :: Bucket -> String
displayBucket (Bucket id' name amount) = printf "(%d) %s - %f" id' name amount

changeBucketAmount :: Bucket -> Float -> Bucket
changeBucketAmount b x = b & bucketAmount %~ (+ x)

instance DomainObject Bucket where
  idOf b = b ^. bucketId

class BucketRepo repo where
  createBucket :: repo -> Bucket -> IO Bucket
  readBucket :: repo -> Int -> IO Bucket
  updateBucket :: repo -> Bucket -> IO ()
  deleteBucket :: repo -> Int -> IO ()
