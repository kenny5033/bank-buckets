module BankBuckets.Commands.Bucket
  ( startBucket,
    depositIntoBucket,
    withdrawFromBucket,
    doInterBucketTransaction,
  )
where

import BankBuckets.Domain.Bucket
import Control.Lens

startBucket :: String -> Float -> Bucket
startBucket name portion =
  Bucket
    { _bucketId = 0,
      _bucketName = name,
      _bucketAmount = 0,
      _bucketPortion = portion,
      _bucketSubBuckets = []
    }

depositIntoBucket :: Float -> Bucket -> Bucket
depositIntoBucket x b = b & bucketAmount %~ (+ x)

withdrawFromBucket :: Float -> Bucket -> Bucket
withdrawFromBucket x b = b & bucketAmount %~ (+ (-x))

doInterBucketTransaction :: Float -> (Bucket, Bucket) -> (Bucket, Bucket)
doInterBucketTransaction x (fromB, toB) =
  (withdrawFromBucket x fromB, depositIntoBucket x toB)
