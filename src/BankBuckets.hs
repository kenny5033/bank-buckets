{-# LANGUAGE MultilineStrings #-}

module BankBuckets (run) where

import BankBuckets.Domain.Bucket
import BankBuckets.Storage.SQLite (newBucketRepo)
import Data.Maybe
import Database.SQLite.Simple (open)
import Text.Read (readMaybe)

version :: String
version = "0.1"

run :: IO ()
run = do
  conn <- open ":memory:"
  bucketRepo <- newBucketRepo conn

  putStrLn $ "BankBuckets v" ++ version

  buckets <- collectAllBuckets bucketRepo
  mapM_ (putStrLn . displayBucket) buckets

  putStrLn
    """
    What would you like to do?
    1) Add new bucket
    """
  _action <- getLine
  _ <- case _action of
    "1" -> do
      putStrLn "What should the bucket be called?"
      _newName <- getLine
      let b = newBucket _newName

      putStrLn "How much is in the bucket?"
      _newAmount <- getLine
      let maybeNewAmount = readMaybe _newAmount :: Maybe Float
      let b' = changeBucketAmount b $ fromMaybe 0 maybeNewAmount

      _ <- createBucket bucketRepo b'
      pure ()
    _ -> do pure ()

  buckets <- collectAllBuckets bucketRepo
  mapM_ (putStrLn . displayBucket) buckets
