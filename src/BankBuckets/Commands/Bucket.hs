module BankBuckets.Commands.Bucket
  ( depositIntoBucket,
    withdrawFromBucket,
    doInterBucketTransaction,
    deleteBucket,
  )
where

import BankBuckets.Domain.Bucket
import Control.Lens
import Data.Maybe

doInterBucketTransaction :: Bucket -> (Bucket, Bucket) -> Float -> Bucket
doInterBucketTransaction root (fromB, toB) x =
  directDepositIntoBucket
    (withdrawFromBucket root (== fromB) x)
    (== toB)
    x

deleteBucket :: Bucket -> (Bucket -> Bool) -> Bucket
deleteBucket root condition = do
  let collapses = map collapseBucket $ root ^. bucketSubBuckets
  let newSubs = mapMaybe snd collapses
  root & bucketAmount %~ (+ (sum $ map fst collapses)) & bucketSubBuckets .~ map (`deleteBucket` condition) newSubs
  where
    collapseBucket :: Bucket -> (Float, Maybe Bucket)
    collapseBucket b
      | condition b = (b ^. bucketParentalAmount, Nothing)
      | otherwise = (0, Just b)
