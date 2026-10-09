module BankBuckets.Domain.Bucket (Bucket (..), displayBucket) where

import BankBuckets.Common (DomainObject (..))

data Bucket = Bucket
  { bucketId :: Int,
    bucketName :: String,
    bucketAmount :: Float
  }
  deriving (Show)

displayBucket :: Bucket -> String
displayBucket (Bucket _ name amount) = name ++ " - " ++ show amount

instance DomainObject Bucket where
  idOf b = bucketId b
