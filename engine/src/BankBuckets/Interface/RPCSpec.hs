{-# LANGUAGE DeriveAnyClass #-}

module BankBuckets.Interface.RPCSpec (PingRequest (..), AddBucketRequest (..)) where

import Data.Aeson
import GHC.Generics

newtype PingRequest = PingRequest (Maybe [Value])
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AddBucketRequest = AddBucketRequest
  { parentName :: String,
    name :: String
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
