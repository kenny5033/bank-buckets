{-# LANGUAGE TemplateHaskell #-}

module BankBuckets.Domain.Bucket
  ( bucketName,
    bucketAmount,
    bucketPortion,
    bucketSubBuckets,
    Bucket (..),
    bucketParentalPortion,
    addSubBucket,
    newBucket,
    findBucket,
    depositIntoBucket,
    withdrawFromBucket,
    bucketParentalAmount,
    updateSubBucket,
  )
where

import Control.Lens as Lens
import Data.Aeson
import Data.Foldable
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

bucketParentalAmount :: Getter Bucket Float
bucketParentalAmount = Lens.to (\b -> b ^. bucketAmount + sum (map (^. bucketParentalAmount) $ b ^. bucketSubBuckets))

updateBucketFromRoot :: (Monad m) => Bucket -> (Bucket -> Bool) -> (Bucket -> m Bucket) -> m Bucket
updateBucketFromRoot root targetCondition targetAction
  | targetCondition root = targetAction root
  | otherwise =
      mUpdatedChildren >>= (\subs -> pure $ root & bucketSubBuckets .~ subs)
  where
    mUpdatedChildren = traverse (\b -> updateBucketFromRoot b targetCondition targetAction) $ root ^. bucketSubBuckets

addSubBucket :: Bucket -> (Bucket -> Bool) -> Bucket -> Either String Bucket
addSubBucket root parentCondition child = updateBucketFromRoot root parentCondition updateParent
  where
    updateParent parent =
      if child ^. bucketPortion > parent ^. bucketParentalPortion
        then Left "Child portion would push total child portions over 100%"
        else Right $ parent & bucketSubBuckets %~ (child :)

depositIntoBucket :: Bucket -> (Bucket -> Bool) -> Float -> Bucket
depositIntoBucket root condition amount =
  runIdentity $
    updateBucketFromRoot root condition (Identity . deposit amount)
  where
    deposit x b = b & bucketAmount %~ (+ parentAmount) & bucketSubBuckets %~ map (\b' -> deposit (amountPerPortion * b' ^. bucketPortion) b')
      where
        amountPerPortion = x / 100
        parentAmount = amountPerPortion * b ^. bucketParentalPortion

withdrawFromBucket :: Bucket -> (Bucket -> Bool) -> Float -> Bucket
withdrawFromBucket root condition amount =
  runIdentity $
    updateBucketFromRoot root condition (\b -> Identity $ b & bucketAmount %~ (+ (-amount)))

updateSubBucket :: Bucket -> (Bucket -> Bool) -> (Bucket -> Bucket) -> Either String Bucket
updateSubBucket root condition tr
  | condition root = Right $ tr root
  | otherwise = do
      case traverse (\b -> updateSubBucket b condition tr) $ root ^. bucketSubBuckets of
        Left err -> Left err
        Right newSubs -> do
          let newRoot = root & bucketSubBuckets .~ newSubs
          let subPortionSum = sum $ newRoot ^.. bucketSubBuckets . each . bucketPortion
          if subPortionSum < 0 || subPortionSum > 100
            then
              Left "Update would violate subs' sum percentages being in [0, 100]"
            else
              Right newRoot

findBucket :: Bucket -> (Bucket -> Bool) -> Maybe Bucket
findBucket root condition
  | condition root = pure root
  | otherwise = asum subResults
  where
    subResults = (`findBucket` condition) <$> root ^. bucketSubBuckets
