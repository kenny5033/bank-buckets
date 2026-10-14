module BankBuckets.Commands.Bucket
  ( startBucket,
  )
where

import BankBuckets.Domain.Bucket
import Data.Time

startBucket :: String -> Float -> Int -> Bucket
startBucket name portion periodDays =
  Bucket
    { _bucketId = 0,
      _bucketName = name,
      _bucketAmount = 0,
      _bucketPortion = portion,
      _bucketPeriod = secondsToNominalDiffTime $ fromIntegral periodDays
    }
