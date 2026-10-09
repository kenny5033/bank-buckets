module BankBuckets (run) where

import BankBuckets.Common (DomainRepo (add, load), InMemoryRepo (InMemoryRepo))
import BankBuckets.Domain.Bucket
import Data.IORef (newIORef)
import Data.Map qualified as Map

run :: IO ()
run = do
  putStrLn "Hello, BankBuckets!"

  let b = Bucket {bucketId = 1, bucketAmount = 0, bucketName = "General Savings"}
  r <- InMemoryRepo <$> newIORef Map.empty
  add r b
  loadedB <- load r $ bucketId b

  let msg = case loadedB of
        Just _b -> displayBucket _b
        Nothing -> "Couldn't load for ID " ++ (show $ bucketId b)

  putStrLn msg
