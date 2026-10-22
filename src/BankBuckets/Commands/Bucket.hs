module BankBuckets.Commands.Bucket
  ( startBucket,
    resolveBucket,
    depositIntoBucket,
    withdrawFromBucket,
    doInterBucketTransaction,
    updateBucketTracking,
    daysUntilPayDate,
    portionPerWeek,
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

depositIntoBucket :: Float -> Bucket -> Bucket
depositIntoBucket x b = b & bucketAmount %~ (+ x)

withdrawFromBucket :: Float -> Bucket -> Bucket
withdrawFromBucket x b = b & bucketAmount %~ (+ (-x))

doInterBucketTransaction :: Float -> (Bucket, Bucket) -> (Bucket, Bucket)
doInterBucketTransaction x (fromB, toB) =
  (withdrawFromBucket x fromB, depositIntoBucket x toB)

updateBucketTracking :: Bucket -> Int -> Float -> Bucket
updateBucketTracking b newPeriodDays newPortion =
  b
    & (bucketPeriod .~ fromIntegral newPeriodDays * nominalDay)
    & (bucketPortion .~ newPortion)

daysUntilPayDate :: UTCTime -> Bucket -> Int
daysUntilPayDate fromTime b = round $ diffUTCTime nextResolutionTime fromTime / nominalDay
  where
    nextResolutionTime = addUTCTime (b ^. bucketPeriod) (b ^. bucketLastResolvedDateTime)

portionPerWeek :: Bucket -> Float
portionPerWeek b = (b ^. bucketPortion / daysInPeriod) * 7
  where
    daysInPeriod = realToFrac $ b ^. bucketPeriod / nominalDay :: Float
