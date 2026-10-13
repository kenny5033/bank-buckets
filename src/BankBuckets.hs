module BankBuckets (run) where

import BankBuckets.Domain.Bucket
import BankBuckets.Storage.SQLite (newBucketRepo)
import Database.SQLite.Simple (open)

version :: String
version = "0.1"

run :: IO ()
run = do
  putStrLn $ "BankBuckets v" ++ version

  conn <- open ":memory:"
  bucketRepo <- newBucketRepo conn
  buckets <- collectAllBuckets bucketRepo
  mapM_ (putStrLn . displayBucket) buckets

  putStrLn "Bye"
