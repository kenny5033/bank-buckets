{-# LANGUAGE MultilineStrings #-}

module BankBuckets (run) where

import BankBuckets.Commands.Bucket
import BankBuckets.Domain.Bucket
import BankBuckets.Storage.SQLite (newBucketRepo)
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Time
import Database.SQLite.Simple (open)
import System.Exit
import Text.Printf (printf)
import Text.Read (readMaybe)

version :: String
version = "0.1"

isDevelopment :: Bool
isDevelopment = False

run :: IO ()
run = do
  conn <- open (if isDevelopment then ":memory:" else "bank_buckets.db")
  bucketRepo <- newBucketRepo conn

  putStrLn $ "BankBuckets v" ++ version

  forever
    ( do
        now <- getCurrentTime
        _buckets <- collectAllBuckets bucketRepo
        let buckets = map (resolveBucket now) _buckets
        let sumAllocations = foldr (\b acc -> acc + b ^. bucketPortion) 0 buckets
        let sumAmount = foldr (\b acc -> acc + b ^. bucketAmount) 0 buckets

        putStr "\x1b[H\x1b[2J"
        putStrLn $ displayTotalAllocation sumAllocations
        putStrLn $ displayTotalSaved sumAmount
        mapM_ (putStrLn . displayBucket) buckets

        putStrLn
          """\n
          What would you like to do?
          1) Add new bucket
          2) Deposit into bucket
          3) Withdraw from bucket
          4) Inter-Bucket Transaction
          q) Exit
          """
        _action <- getLine
        case _action of
          "1" -> addNewBucketUI bucketRepo
          "2" -> depositIntoBucketUI bucketRepo buckets
          "3" -> withdrawFromBucketUI bucketRepo buckets
          "4" -> interBucketTransactionUI bucketRepo buckets
          "q" -> exitSuccess
          _ -> do pure ()
    )

displayBucket :: Bucket -> String
displayBucket (Bucket id' name amount portion period _) = printf "(%d) %s | %f <- %f / %s" id' name amount portion (displayPeriod period)

displayPeriod :: NominalDiffTime -> String
displayPeriod dt = printf "%dw %dd" (div days 7) (mod days 7)
  where
    days = round $ dt / nominalDay :: Int

displayTotalAllocation :: Float -> String
displayTotalAllocation = printf "Total Allocated: %f"

displayTotalSaved :: Float -> String
displayTotalSaved = printf "Total Saved: %f"

addNewBucketUI :: (BucketRepo r) => r -> IO ()
addNewBucketUI bucketRepo = do
  putStrLn "What should the bucket be called?"
  newName <- getLine

  putStrLn "What is the period of this bucket in days?"
  _newPeriodDays <- getLine
  let newPeriodDays = fromMaybe 7 (readMaybe _newPeriodDays :: Maybe Int)

  putStrLn "How much should go in the bucket every period?"
  _newPortion <- getLine
  let newPortion = fromMaybe 0 (readMaybe _newPortion :: Maybe Float)

  now <- getCurrentTime
  _ <- createBucket bucketRepo $ startBucket newName newPortion newPeriodDays now
  pure ()

depositIntoBucketUI :: (BucketRepo r) => r -> [Bucket] -> IO ()
depositIntoBucketUI r buckets = do
  putStrLn "Which bucket (ID)?"
  _targetBucketId <- getLine
  let maybeTargetBucketId = readMaybe _targetBucketId :: Maybe Int
  case maybeTargetBucketId of
    Nothing -> putStrLn "Please input a valid bucket ID"
    Just targetBucketId -> do
      let maybeBucket = find (\b -> b ^. bucketId == targetBucketId) buckets
      case maybeBucket of
        Nothing -> putStrLn "Couldn't find that bucket"
        Just bucket -> do
          putStrLn "How much?"
          _amount <- getLine
          let amount = fromMaybe 0 (readMaybe _amount :: Maybe Float)
          updateBucket r $ depositIntoBucket amount bucket

withdrawFromBucketUI :: (BucketRepo r) => r -> [Bucket] -> IO ()
withdrawFromBucketUI r buckets = do
  putStrLn "Which bucket (ID)?"
  _targetBucketId <- getLine
  let maybeTargetBucketId = readMaybe _targetBucketId :: Maybe Int
  case maybeTargetBucketId of
    Nothing -> putStrLn "Please input a valid bucket ID"
    Just targetBucketId -> do
      let maybeBucket = find (\b -> b ^. bucketId == targetBucketId) buckets
      case maybeBucket of
        Nothing -> putStrLn "Couldn't find that bucket"
        Just bucket -> do
          putStrLn "How much?"
          _amount <- getLine
          let amount = fromMaybe 0 (readMaybe _amount :: Maybe Float)
          updateBucket r $ withdrawFromBucket amount bucket

interBucketTransactionUI :: (BucketRepo r) => r -> [Bucket] -> IO ()
interBucketTransactionUI r buckets = do
  putStrLn "From which bucket (ID)?"
  _fromBucketId <- getLine
  let fromBucketId = fromMaybe 0 (readMaybe _fromBucketId :: Maybe Int)
  let maybeFromBucket = find (\b -> b ^. bucketId == fromBucketId) buckets

  case maybeFromBucket of
    Nothing -> putStrLn "Couldn't find that bucket"
    Just fromBucket -> do
      putStrLn "To which bucket (ID)?"
      _toBucketId <- getLine
      let toBucketId = fromMaybe 0 (readMaybe _toBucketId :: Maybe Int)
      let maybeToBucket = find (\b -> b ^. bucketId == toBucketId) buckets

      case maybeToBucket of
        Nothing -> putStrLn "Couldn't find that bucket"
        Just toBucket -> do
          putStrLn "How much?"
          _amount <- getLine
          let amount = fromMaybe 0 (readMaybe _amount :: Maybe Float)
          let (newFromBucket, newToBucket) = doInterBucketTransaction amount (fromBucket, toBucket)
          updateBucket r newFromBucket
          updateBucket r newToBucket
