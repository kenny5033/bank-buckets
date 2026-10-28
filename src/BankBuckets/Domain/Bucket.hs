{-# LANGUAGE TemplateHaskell #-}

module BankBuckets.Domain.Bucket
  ( bucketId,
    bucketName,
    bucketAmount,
    bucketPortion,
    bucketSubBuckets,
    BucketRepo (..),
    Bucket (..),
  )
where

import Control.Lens
import Data.Aeson
import GHC.Generics

data Bucket = Bucket
  { _bucketId :: Int,
    _bucketName :: String,
    _bucketAmount :: Float,
    _bucketPortion :: Float,
    _bucketSubBuckets :: [Bucket]
  }
  deriving (Show, Generic)

instance ToJSON Bucket

instance FromJSON Bucket

makeLenses ''Bucket

class BucketRepo repo where
  createBucket :: repo -> Bucket -> IO Bucket
  readBucket :: repo -> Int -> IO (Maybe Bucket)
  updateBucket :: repo -> Bucket -> IO ()
  deleteBucket :: repo -> Int -> IO ()

  collectAllBuckets :: repo -> IO [Bucket]
