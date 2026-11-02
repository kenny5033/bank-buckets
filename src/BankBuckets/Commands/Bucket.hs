module BankBuckets.Commands.Bucket
  ( depositIntoBucket,
    withdrawFromBucket,
    doInterBucketTransaction,
  )
where

import BankBuckets.Domain.Bucket
import Control.Lens

depositIntoBucket :: Float -> Bucket -> Bucket
depositIntoBucket x b =
  b & bucketAmount %~ (+ parentAmount) & bucketSubBuckets %~ map depositIntoSub
  where
    parentAmount = amountPerPortion * b ^. bucketParentalPortion
    depositIntoSub sub = depositIntoBucket (amountPerPortion * sub ^. bucketPortion) sub
    amountPerPortion = x / 100

withdrawFromBucket :: Float -> Bucket -> Bucket
withdrawFromBucket x b = b & bucketAmount %~ (+ (-x))

doInterBucketTransaction :: Float -> (Bucket, Bucket) -> (Bucket, Bucket)
doInterBucketTransaction x (fromB, toB) =
  (withdrawFromBucket x fromB, depositIntoBucket x toB)
