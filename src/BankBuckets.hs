{-# LANGUAGE MultilineStrings #-}

module BankBuckets (run) where

import BankBuckets.Commands.Bucket
import BankBuckets.Domain.Bucket
import BankBuckets.Domain.Portfolio
import BankBuckets.Storage.SQLite
import Control.Lens
import Data.Maybe
import Database.SQLite.Simple (open)
import Text.Printf (printf)
import Text.Read (readMaybe)

version :: String
version = "0.1"

isDevelopment :: Bool
isDevelopment = True

mainLoop :: Bucket -> IO Bucket
mainLoop rootBucket = do
  case isDevelopment of
    False -> putStr "\x1b[H\x1b[2J"
    True -> putStrLn "---------------"

  putStrLn $ displayBucket 0 rootBucket

  putStrLn
    """\n
    What would you like to do?
    a Add new bucket
    d) Deposit
    w) Withdraw
    t) Do inter-bucket transaction
    x) Delete bucket
    u) Update bucket
    q) Exit
    """
  _action <- getLine
  case _action of
    "a" -> addNewBucketUI rootBucket >>= mainLoop
    "d" -> depositUI rootBucket >>= mainLoop
    -- "w" -> withdrawUI bucketRepo baseBuckets
    -- "t" -> interBucketTransactionUI bucketRepo baseBuckets
    -- "x" -> deleteBucketUI bucketRepo
    -- "u" -> updateBucketUI bucketRepo
    "q" -> pure rootBucket
    _ -> mainLoop rootBucket

run :: IO ()
run = do
  conn <- open (if isDevelopment then ":memory:" else "bank_buckets.db")
  portfolioRepo <- newPortfolioSQLiteRepo conn

  putStrLn $ "BankBuckets v" ++ version

  _portfolio <- readPortfolio portfolioRepo 1
  let portfolio = fromMaybe (newPortfolio "main") _portfolio

  newRoot <- mainLoop $ portfolio ^. portfolioRootBucket

  let finalPortfolio = portfolio & portfolioRootBucket .~ newRoot
  _ <- upsertPortfolio portfolioRepo finalPortfolio

  pure ()

displayBucket :: Int -> Bucket -> String
displayBucket
  level
  Bucket
    { _bucketName = name,
      _bucketAmount = amount,
      _bucketPortion = portion,
      _bucketSubBuckets = subBuckets
    } =
    concat
      ( printf
          "%s%s | %f <- %f%%\n"
          indentation
          name
          amount
          portion
          : map (displayBucket (level + 1)) subBuckets
      )
    where
      indentation = (concat . replicate level) "  "

collectBucketNameUI :: IO String
collectBucketNameUI = do
  putStrLn "What should this bucket be called?"
  getLine

collectBucketPortionUI :: IO Float
collectBucketPortionUI = do
  putStrLn "How much should this bucket receive from its parent (0-100%)?"
  _newPortion <- filter (/= '%') <$> getLine
  let newPortion = fromMaybe 0 (readMaybe _newPortion :: Maybe Float)
  pure newPortion

selectBucketUI :: Bucket -> IO (Maybe Bucket)
selectBucketUI root = do
  putStrLn "What's the bucket's name?"
  targetBucketName <- getLine
  let maybeBucket = findBucketFromRoot ((== targetBucketName) . (^. bucketName)) root
  pure maybeBucket

addNewBucketUI :: Bucket -> IO Bucket
addNewBucketUI root = do
  _parent <- selectBucketUI root
  case _parent of
    Nothing -> pure root
    Just parent -> do
      name <- collectBucketNameUI
      portion <- collectBucketPortionUI

      let maybeNewBucket = newBucket name portion
      case maybeNewBucket of
        Left err -> do
          putStrLn err
          pure root
        Right b -> case bucketAddSubBucket root (== parent) b of
          Left err' -> do
            putStrLn err'
            pure root
          Right b' -> pure b'

depositUI :: Bucket -> IO Bucket
depositUI root = do
  putStrLn "How much?"
  _amount <- getLine
  let amount = fromMaybe 0 (readMaybe _amount :: Maybe Float)
  pure $ depositIntoBucket amount root
