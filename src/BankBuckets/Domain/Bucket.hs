{-# LANGUAGE TemplateHaskell #-}

module BankBuckets.Domain.Bucket (Bucket (..), displayBucket, changeBucketAmount) where

import BankBuckets.Common (DomainObject (..))
import Control.Lens

data Bucket = Bucket
  { _bucketId :: Int,
    _bucketName :: String,
    _bucketAmount :: Float
  }
  deriving (Show)

$(makeLenses ''Bucket)

instance DomainObject Bucket where
  idOf b = b ^. bucketId

displayBucket :: Bucket -> String
displayBucket (Bucket _ name amount) = name ++ " - " ++ show amount

changeBucketAmount :: Bucket -> Float -> Bucket
changeBucketAmount b x = b & bucketAmount %~ (+ x)
