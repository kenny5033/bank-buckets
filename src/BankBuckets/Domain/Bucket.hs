{-# LANGUAGE TemplateHaskell #-}

module BankBuckets.Domain.Bucket
  ( bucketName,
    bucketAmount,
    bucketPortion,
    bucketSubBuckets,
    Bucket (..),
    bucketParentalPortion,
    bucketAddSubBucket,
    newBucket,
    findBucketFromRoot,
    updateBucketFromRoot,
  )
where

import Control.Lens as Lens
import Data.Aeson
import GHC.Generics

data Bucket = Bucket
  { _bucketName :: String,
    _bucketAmount :: Float,
    _bucketPortion :: Float,
    _bucketSubBuckets :: [Bucket]
  }
  deriving (Show, Generic, Eq)

instance ToJSON Bucket

instance FromJSON Bucket

makeLenses ''Bucket

newBucket :: String -> Float -> Either String Bucket
newBucket name portion =
  if portion < 0 || portion > 100
    then
      Left "Bucket portion must be within [0%, 100%]"
    else Right freshBucket
  where
    freshBucket =
      Bucket
        { _bucketName = name,
          _bucketAmount = 0,
          _bucketPortion = portion,
          _bucketSubBuckets = []
        }

-- The portion of a bucket as a parent of other buckets, i.e.
--   100% - subBucket_0 portion - ... - subBucket_n portion
-- How much this bucket should receive after the children have had their share
bucketParentalPortion :: Getter Bucket Float
bucketParentalPortion = Lens.to (\b -> 100 - sum (map (^. bucketPortion) $ b ^. bucketSubBuckets))

updateBucketFromRoot :: Bucket -> (Bucket -> Bool) -> (Bucket -> Bucket) -> Bucket
updateBucketFromRoot root targetCondition targetAction
  | targetCondition root = targetAction root
  | otherwise =
      root
        & bucketSubBuckets %~ map (\b -> updateBucketFromRoot b targetCondition targetAction)

bucketAddSubBucket :: Bucket -> (Bucket -> Bool) -> Bucket -> Either String Bucket
bucketAddSubBucket root parentCondition child
  | parentCondition root =
      if child ^. bucketPortion > root ^. bucketParentalPortion
        then
          Left "Child portion would push total child portions over 100%"
        else
          Right $ root & bucketSubBuckets %~ (child :)
  | otherwise =
      case traverse (\b -> bucketAddSubBucket b parentCondition child) $ root ^. bucketSubBuckets of
        Left e -> Left e
        Right subs -> Right $ root & bucketSubBuckets .~ subs

findBucketFromRoot :: (Bucket -> Bool) -> Bucket -> Maybe Bucket
findBucketFromRoot condition root =
  if condition root
    then
      pure root
    else case subResults of
      Just b : _ -> pure b
      _ -> Nothing
  where
    subResults = map (findBucketFromRoot condition) $ root ^. bucketSubBuckets
