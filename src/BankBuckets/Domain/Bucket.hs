module Bucket (Bucket) where

data Bucket = Bucket
  { name :: String,
    amount :: Float
  }
  deriving (Show)
