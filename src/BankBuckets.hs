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
        let weekAvgAllocations = foldr (\b acc -> acc + portionPerWeek b) 0 buckets
        let sumAmount = foldr (\b acc -> acc + b ^. bucketAmount) 0 buckets

        putStr "\x1b[H\x1b[2J"
        putStrLn $ displayTotalAllocation weekAvgAllocations
        putStrLn $ displayTotalSaved sumAmount
        let bucketDisplays = map displayBucket buckets
        mapM_ (putStrLn . ($ now)) bucketDisplays

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
          "u" -> updateBucketUI bucketRepo buckets
          "q" -> exitSuccess
          _ -> do pure ()
    )

displayBucket :: Bucket -> UTCTime -> String
displayBucket b@(Bucket id' name amount portion period _) atTime =
  printf
    "(%d) %s | %f <- %f / %s in %dd"
    id'
    name
    amount
    portion
    (displayPeriod period)
    (daysUntilPayDate atTime b)

displayPeriod :: NominalDiffTime -> String
displayPeriod dt = printf "%dw %dd" (div days 7) (mod days 7)
  where
    days = round $ dt / nominalDay :: Int

displayTotalAllocation :: Float -> String
displayTotalAllocation = printf "Total Allocated: %f / week"

displayTotalSaved :: Float -> String
displayTotalSaved = printf "Total Saved: %f"

updateBucketFieldsUI :: IO (Int, Float)
updateBucketFieldsUI = do
  putStrLn "What is the period of this bucket in days?"
  _newPeriodDays <- getLine
  let newPeriodDays = fromMaybe 7 (readMaybe _newPeriodDays :: Maybe Int)

  putStrLn "How much should go in the bucket every period?"
  _newPortion <- getLine
  let newPortion = fromMaybe 0 (readMaybe _newPortion :: Maybe Float)

  pure (newPeriodDays, newPortion)

addNewBucketUI :: (BucketRepo r) => r -> IO ()
addNewBucketUI bucketRepo = do
  putStrLn "What should the bucket be called?"
  newName <- getLine

  (newPeriodDays, newPortion) <- updateBucketFieldsUI
  now <- getCurrentTime

  _ <- createBucket bucketRepo $ startBucket newName newPortion newPeriodDays now
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

updateBucketUI :: (BucketRepo r) => r -> [Bucket] -> IO ()
updateBucketUI r buckets = do
  maybeTargetBucket <-
    selectBucketUI
      "Which bucket (ID) should be updated?"
      "Couldn't find that bucket"
      buckets

  case maybeTargetBucket of
    Nothing -> pure ()
    Just targetBucket -> do
      (newPeriodDays, newPortion) <- updateBucketFieldsUI
      updateBucket r $ updateBucketTracking targetBucket newPeriodDays newPortion
