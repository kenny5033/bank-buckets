{-# LANGUAGE MultilineStrings #-}

module BankBuckets (run) where

import BankBuckets.Commands.Bucket
import BankBuckets.Domain.Bucket
import BankBuckets.Storage.SQLite (newBucketRepo)
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Maybe
import Database.SQLite.Simple (open)
import System.Exit
import Text.Printf (printf)
import Text.Read (readMaybe)

version :: String
version = "0.1"

isDevelopment :: Bool
isDevelopment = True

run :: IO ()
run = do
  conn <- open (if isDevelopment then ":memory:" else "bank_buckets.db")
  bucketRepo <- newBucketRepo conn

  putStrLn $ "BankBuckets v" ++ version

  forever
    ( do
        buckets <- collectAllBuckets bucketRepo

        putStr "\x1b[H\x1b[2J"
        let bucketDisplays = map (displayBucket 0) buckets
        mapM_ putStrLn bucketDisplays

        putStrLn
          """\n
          What would you like to do?
          a) Add new bucket
          d) Deposit into bucket
          w) Withdraw from bucket
          t) Do inter-bucket transaction
          x) Delete bucket
          u) Update bucket
          q) Exit
          """
        _action <- getLine
        case _action of
          "a" -> addNewBucketUI bucketRepo
          "d" -> depositIntoBucketUI bucketRepo buckets
          "w" -> withdrawFromBucketUI bucketRepo buckets
          "t" -> interBucketTransactionUI bucketRepo buckets
          "x" -> deleteBucketUI bucketRepo
          "q" -> exitSuccess
          _ -> do pure ()
    )

displayBucket :: Int -> Bucket -> String
displayBucket level (Bucket id' name amount portion subBuckets) =
  concat
    ( printf
        "%s(%d) %s | %f <- %f%%"
        indentation
        id'
        name
        amount
        portion
        : map (displayBucket (level + 1)) subBuckets
    )
  where
    indentation = (concat . replicate level) " "

updateBucketFieldsUI :: IO Float
updateBucketFieldsUI = do
  putStrLn "How much should go into this bucket from it's parent? (0%-100%) "
  _newPortion <- getLine
  let newPortion = fromMaybe 0 (readMaybe _newPortion :: Maybe Float)

  pure newPortion

addNewBucketUI :: (BucketRepo r) => r -> IO ()
addNewBucketUI bucketRepo = do
  putStrLn "What should the bucket be called?"
  newName <- getLine

  newPortion <- updateBucketFieldsUI

  _ <- createBucket bucketRepo $ startBucket newName newPortion
  pure ()

selectBucketUI :: String -> String -> [Bucket] -> IO (Maybe Bucket)
selectBucketUI prompt failure_message buckets = do
  putStrLn prompt
  _targetBucketId <- getLine
  let targetBucketId = fromMaybe 0 (readMaybe _targetBucketId :: Maybe Int)
  let maybeBucket = find (\b -> b ^. bucketId == targetBucketId) buckets
  when (isNothing maybeBucket) $ putStrLn failure_message
  pure maybeBucket

depositIntoBucketUI :: (BucketRepo r) => r -> [Bucket] -> IO ()
depositIntoBucketUI r buckets = do
  maybeBucket <- selectBucketUI "Which bucket (ID)?" "Couldn't find that bucket" buckets
  case maybeBucket of
    Just bucket -> do
      putStrLn "How much?"
      _amount <- getLine
      let amount = fromMaybe 0 (readMaybe _amount :: Maybe Float)
      updateBucket r $ depositIntoBucket amount bucket
    Nothing -> pure ()

withdrawFromBucketUI :: (BucketRepo r) => r -> [Bucket] -> IO ()
withdrawFromBucketUI r buckets = do
  maybeBucket <- selectBucketUI "Which bucket (ID)?" "Couldn't find that bucket" buckets
  case maybeBucket of
    Nothing -> pure ()
    Just bucket -> do
      putStrLn "How much?"
      _amount <- getLine
      let amount = fromMaybe 0 (readMaybe _amount :: Maybe Float)
      updateBucket r $ withdrawFromBucket amount bucket

interBucketTransactionUI :: (BucketRepo r) => r -> [Bucket] -> IO ()
interBucketTransactionUI r buckets = do
  maybeFromBucket <- selectBucketUI "From which bucket (ID)?" "Couldn't find that bucket" buckets
  case maybeFromBucket of
    Nothing -> pure ()
    Just fromBucket -> do
      maybeToBucket <- selectBucketUI "To which bucket (ID)?" "Couldn't find that bucket" buckets
      case maybeToBucket of
        Nothing -> pure ()
        Just toBucket -> do
          putStrLn "How much?"
          _amount <- getLine
          let amount = fromMaybe 0 (readMaybe _amount :: Maybe Float)
          let (newFromBucket, newToBucket) = doInterBucketTransaction amount (fromBucket, toBucket)
          updateBucket r newFromBucket
          updateBucket r newToBucket

deleteBucketUI :: (BucketRepo r) => r -> IO ()
deleteBucketUI r = do
  putStrLn "Which bucket (ID) should be deleted?"
  _targetBucketId <- getLine
  let targetBucketId = fromMaybe 0 (readMaybe _targetBucketId :: Maybe Int)
  deleteBucket r targetBucketId
