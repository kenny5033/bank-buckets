module BankBuckets.Commands.Bucket
  ( startBucket,
    resolveBucket,
  )
where

import BankBuckets.Domain.Bucket
import Control.Lens
import Data.Time

startBucket :: String -> Float -> Int -> UTCTime -> Bucket
startBucket name portion periodDays startedAt =
  Bucket
    { _bucketId = 0,
      _bucketName = name,
      _bucketAmount = 0,
      _bucketPortion = portion,
      _bucketPeriod = fromIntegral periodDays * nominalDay,
      _bucketLastResolvedDateTime = startedAt
    }

resolveBucket :: UTCTime -> Bucket -> Bucket
resolveBucket resolveAt b =
  if diff >= b ^. bucketPeriod
    then do b & increaseAmount & setResolvedTime
    else b
  where
    diff = diffUTCTime resolveAt $ b ^. bucketLastResolvedDateTime
    numPeriodsToProcess = floor $ diff / b ^. bucketPeriod :: Int
    increaseAmount =
      bucketAmount
        %~ (+ (b ^. bucketPortion * fromIntegral numPeriodsToProcess))
    setResolvedTime =
      bucketLastResolvedDateTime
        %~ addUTCTime
          (realToFrac numPeriodsToProcess * (b ^. bucketPeriod) :: NominalDiffTime)
