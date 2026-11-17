{-# LANGUAGE DeriveAnyClass #-}

module BankBuckets.Interface.RPCSpec (PingRequest (..), AddBucketRequest (..), rpcAddBucket) where

import BankBuckets.Domain.Bucket
import BankBuckets.Domain.Portfolio
import Control.Lens
import Data.Aeson
import GHC.Generics
import Text.Printf

newtype PingRequest = PingRequest (Maybe [Value])
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

rpcGetPortfolioRoot :: (PortfolioRepo r) => r -> Int -> IO (Either String Bucket)
rpcGetPortfolioRoot repo pId = do
  maybePortfolio <- readPortfolio repo pId
  pure $ case maybePortfolio of
    Nothing -> Left $ printf "Couldn't find portfolio id = %d" pId
    Just portfolio -> Right $ portfolio ^. portfolioRootBucket

data AddBucketRequest = AddBucketRequest
  { pId :: Int,
    parentName :: String,
    name :: String,
    portion :: Float
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AddBucketResponse
  = AddBucketSuccess
      { newRootBucket :: Bucket
      }
  | AddBucketFailure
      { message :: String
      }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

rpcAddBucket :: (PortfolioRepo r) => r -> AddBucketRequest -> IO AddBucketResponse
rpcAddBucket repo AddBucketRequest {pId, parentName, name, portion} = do
  maybeRoot <- rpcGetPortfolioRoot repo pId
  let maybeNewRoot = maybeRoot >>= (\root -> getParent root >>= addToParent root)
  pure $ case maybeNewRoot of
    Left err -> AddBucketFailure err
    Right newRoot -> AddBucketSuccess newRoot
  where
    getParent :: Bucket -> Either String Bucket
    getParent root = do
      let maybeParent = findBucket root ((== parentName) . (^. bucketName))

      case maybeParent of
        Nothing -> Left $ printf "Couldn't find parent bucket name = %s" parentName
        Just parent -> Right parent

    addToParent :: Bucket -> Bucket -> Either String Bucket
    addToParent root parent = newBucket name portion >>= addSubBucket root (== parent)
