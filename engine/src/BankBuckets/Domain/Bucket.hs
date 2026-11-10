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
    directDepositIntoBucket,
  )
where

import Control.Lens as Lens
import Control.Monad
import Data.Aeson
import Data.Either
import Data.Fixed
import Data.Foldable
import GHC.Generics

data Bucket = Bucket
  { _bucketName :: String,
    _bucketAmount :: Centi,
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

bucketParentalAmount :: Getter Bucket Centi
bucketParentalAmount = Lens.to (\b -> b ^. bucketAmount + sum (map (^. bucketParentalAmount) $ b ^. bucketSubBuckets))

updateBucketFromRoot :: (Monad m) => Bucket -> (Bucket -> Bool) -> (Bucket -> m Bucket) -> m (Either String Bucket)
updateBucketFromRoot root targetCondition targetAction
  | targetCondition root = do
      let mNewRoot = targetAction root
      newRoot <- mNewRoot
      let newPortion = newRoot ^. bucketPortion
      if newPortion < 0 || newPortion > 100
        then
          pure $ Left "Update would violate the bucket's portion being in [0, 100]"
        else
          pure $ Right newRoot
  | otherwise = do
      let mNewSubs = traverse (\b -> updateBucketFromRoot b targetCondition targetAction) $ root ^. bucketSubBuckets
      newSubs <- mNewSubs
      case sequence newSubs of
        Left err -> pure $ Left err
        Right subs -> do
          let newRoot = root & bucketSubBuckets .~ subs
          let subPortionSum = sum $ newRoot ^.. bucketSubBuckets . each . bucketPortion
          if subPortionSum < 0 || subPortionSum > 100
            then
              pure $ Left "Update would violate subs' total portions being in [0, 100]"
            else
              pure $ Right newRoot

addSubBucket :: Bucket -> (Bucket -> Bool) -> Bucket -> Either String Bucket
addSubBucket root parentCondition child = join $ updateBucketFromRoot root parentCondition updateParent
  where
    updateParent parent =
      if child ^. bucketPortion > parent ^. bucketParentalPortion
        then Left "New sub's portion would push subs' total portions over 100%"
        else Right $ parent & bucketSubBuckets %~ (child :)

depositIntoBucket :: Bucket -> (Bucket -> Bool) -> Centi -> Bucket
depositIntoBucket root condition amount =
  fromRight root $ runIdentity $ updateBucketFromRoot root condition (Identity . deposit amount)
  where
    deposit x b = b & bucketAmount %~ (+ parentAmount) & bucketSubBuckets %~ map (\b' -> deposit (x * realToFrac (b' ^. bucketPortion / 100)) b')
      where
        adjustedPortion = b ^. bucketParentalPortion / 100
        parentAmount = x * realToFrac adjustedPortion

directDepositIntoBucket :: Bucket -> (Bucket -> Bool) -> Centi -> Bucket
directDepositIntoBucket root condition amount =
  fromRight root $ runIdentity $ updateBucketFromRoot root condition (\b -> Identity $ b & bucketAmount %~ (+ amount))

withdrawFromBucket :: Bucket -> (Bucket -> Bool) -> Centi -> Bucket
withdrawFromBucket root condition amount =
  fromRight root $
    runIdentity $
      updateBucketFromRoot root condition (\b -> Identity $ b & bucketAmount %~ (+ (-amount)))

updateSubBucket :: Bucket -> (Bucket -> Bool) -> (String, Float) -> Either String Bucket
updateSubBucket root condition (newName, newPortion) =
  runIdentity $
    updateBucketFromRoot root condition (\b -> Identity $ b & bucketName .~ newName & bucketPortion .~ newPortion)

findBucket :: Bucket -> (Bucket -> Bool) -> Maybe Bucket
findBucket root condition
  | condition root = pure root
  | otherwise = asum subResults
  where
    subResults = (`findBucket` condition) <$> root ^. bucketSubBuckets
