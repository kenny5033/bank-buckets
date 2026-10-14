{-# LANGUAGE MultilineStrings #-}

module BankBuckets (run) where

import BankBuckets.Commands.Bucket
import BankBuckets.Domain.Bucket
import BankBuckets.Storage.SQLite (newBucketRepo)
import Data.Maybe
import Database.SQLite.Simple (open)
import Text.Printf (printf)
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
    "1" -> addNewBucket bucketRepo
    _ -> do pure ()

  buckets <- collectAllBuckets bucketRepo
  mapM_ (putStrLn . displayBucket) buckets

displayBucket :: Bucket -> String
displayBucket (Bucket id' name amount portion period) = printf "(%d) %s | %f <- %f / %s" id' name amount portion (show period)

addNewBucket :: (BucketRepo r) => r -> IO ()
addNewBucket bucketRepo = do
  putStrLn "What should the bucket be called?"
  newName <- getLine

  putStrLn "What is the period of this bucket in days?"
  _newPeriodDays <- getLine
  let newPeriodDays = fromMaybe 7 (readMaybe _newPeriodDays :: Maybe Int)

  putStrLn "How much should go in the bucket every period?"
  _newPortion <- getLine
  let newPortion = fromMaybe 0 (readMaybe _newPortion :: Maybe Float)

  _ <- createBucket bucketRepo $ startBucket newName newPortion newPeriodDays
  pure ()
