module BankBuckets (run) where

import BankBuckets.Domain.Bucket (BucketRepo (createBucket, readBucket, updateBucket), changeBucketAmount, displayBucket, newBucket)
import BankBuckets.Storage.SQLite (newBucketRepo)
import Database.SQLite.Simple (open)

version :: String
version = "0.1"

run :: IO ()
run = do
  putStrLn $ "BankBuckets v" ++ version

  conn <- open ":memory:"
  bucketRepo <- newBucketRepo conn
  b <- createBucket bucketRepo $ newBucket "General Savings"
  b2 <- createBucket bucketRepo $ newBucket "Fun Money"
  putStrLn $ displayBucket b
  putStrLn $ displayBucket b2

  updateBucket bucketRepo $ changeBucketAmount b 10
  b' <- readBucket bucketRepo 1
  putStrLn $ displayBucket b'

  putStrLn "Bye"
