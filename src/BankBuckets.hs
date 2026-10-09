module BankBuckets (run) where

import BankBuckets.Common (DomainObject (idOf), DomainRepo (add, load, update), InMemoryRepo (InMemoryRepo))
import BankBuckets.Domain.Bucket
import Data.IORef (newIORef)
import Data.Map qualified as Map

run :: IO ()
run = do
  putStrLn "Hello, BankBuckets!"

  let b = Bucket {_bucketId = 1, _bucketAmount = 0, _bucketName = "General Savings"}
  r <- InMemoryRepo <$> newIORef Map.empty
  add r b
  update r $ changeBucketAmount b 3.0
  loadedB <- load r $ idOf b

  let msg = case loadedB of
        Just b' -> displayBucket b'
        Nothing -> "Couldn't load for ID " ++ (show $ idOf b)

  putStrLn msg
