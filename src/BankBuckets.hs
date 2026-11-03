{-# LANGUAGE MultilineStrings #-}

module BankBuckets (run) where

import BankBuckets.Commands.Bucket
import BankBuckets.Domain.Bucket
import BankBuckets.Domain.Portfolio
import BankBuckets.Storage.SQLite
import Control.Lens
import Control.Monad
import Data.Maybe
import Database.SQLite.Simple (open)
import Text.Printf (printf)
import Text.Read (readMaybe)

version :: String
version = "0.2"

isDevelopment :: Bool
isDevelopment = True

mainLoop :: Bucket -> [String] -> [Bucket] -> IO Bucket
mainLoop rootBucket errorMessages previousRoots = do
  putStr "\x1b[H\x1b[2J"

  mapM_ putStrLn errorMessages
  unless (null errorMessages) (putStr "\n")

  putStrLn $ displayBucket 0 rootBucket

  putStrLn
    """\n
    What would you like to do?
    a) Add new bucket
    d) Deposit
    w) Withdraw
    t) Do inter-bucket transaction
    x) Delete bucket
    u) Update bucket
    z) Undo last action
    q) Exit
    """
  _action <- getLine
  case _action of
    "a" -> addNewBucketUI rootBucket >>= errorfulContinue
    "d" -> depositUI rootBucket >>= errorfulContinue
    "w" -> withdrawUI rootBucket >>= errorfulContinue
    "t" -> interBucketTransactionUI rootBucket >>= errorfulContinue
    "x" -> deleteUI rootBucket >>= errorfulContinue
    "u" -> updateUI rootBucket >>= errorfulContinue
    "z" -> case previousRoots of
      [] -> mainLoop rootBucket ["No previous actions"] previousRoots
      prevRoot : rest -> mainLoop prevRoot [] rest
    "q" -> pure rootBucket
    _ -> mainLoop rootBucket [] previousRoots
  where
    errorfulContinue :: Either String Bucket -> IO Bucket
    errorfulContinue (Left err) = mainLoop rootBucket [err] previousRoots
    errorfulContinue (Right newRoot) = mainLoop newRoot [] (rootBucket : previousRoots)

run :: IO ()
run = do
  conn <- open (if isDevelopment then ":memory:" else "bank_buckets.db")
  portfolioRepo <- newPortfolioSQLiteRepo conn

  _portfolio <- readPortfolio portfolioRepo 1
  let portfolio = fromMaybe (newPortfolio "main") _portfolio
  let initialRoot = portfolio ^. portfolioRootBucket

  newRoot <- mainLoop initialRoot ["BankBuckets v" ++ version] []

  let finalPortfolio = portfolio & portfolioRootBucket .~ newRoot
  _ <- upsertPortfolio portfolioRepo finalPortfolio

  pure ()

displayBucket :: Int -> Bucket -> String
displayBucket
  level
  b@Bucket
    { _bucketName = name,
      _bucketAmount = amount,
      _bucketSubBuckets = subBuckets,
      _bucketPortion = portion
    } =
    concat
      ( printf
          "%s%-15.15s %10.2f <- %3.2f%%%s\n"
          indentation
          name
          amount
          portion
          ( case b ^. bucketParentalPortion of
              100 -> ""
              p -> " (" ++ show p ++ "%)"
          )
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

selectBucketUI :: Bucket -> String -> IO (Maybe Bucket)
selectBucketUI root prompt = do
  putStrLn prompt
  targetBucketName <- getLine
  let maybeBucket = findBucket root ((== targetBucketName) . (^. bucketName))
  pure maybeBucket

addNewBucketUI :: Bucket -> IO (Either String Bucket)
addNewBucketUI root = do
  _parent <- selectBucketUI root "What is the name of the parent of the new bucket?"
  case _parent of
    Nothing -> pure $ Left "Could not find that bucket"
    Just parent -> do
      name <- collectBucketNameUI
      portion <- collectBucketPortionUI

      let maybeNewBucket = newBucket name portion
      case maybeNewBucket of
        Left err -> pure $ Left err
        Right b -> case addSubBucket root (== parent) b of
          Left err' -> pure $ Left err'
          Right b' -> pure $ Right b'

depositUI :: Bucket -> IO (Either String Bucket)
depositUI root = do
  putStrLn "How much?"
  _amount <- getLine
  let amount = readMaybe _amount :: Maybe Float
  case amount of
    Nothing -> pure $ Left "Could not parse that as a number"
    Just a -> pure $ Right $ depositIntoBucket root (== root) a

withdrawUI :: Bucket -> IO (Either String Bucket)
withdrawUI root = do
  _target <- selectBucketUI root "From which bucket should withdrawl be applied?"
  case _target of
    Nothing -> pure $ Left "Could not find that bucket"
    Just target -> do
      putStrLn "How much?"
      _amount <- getLine
      let amount = readMaybe _amount :: Maybe Float
      case amount of
        Nothing -> pure $ Left "Could not parse that as a number"
        Just a -> pure $ Right $ withdrawFromBucket root (== target) a

interBucketTransactionUI :: Bucket -> IO (Either String Bucket)
interBucketTransactionUI root = do
  _fromTarget <- selectBucketUI root "From which bucket name?"
  case _fromTarget of
    Nothing -> pure $ Left "Could not find that bucket"
    Just fromTarget -> do
      _toTarget <- selectBucketUI root "To which bucket name?"
      case _toTarget of
        Nothing -> pure $ Left "Could not find that bucket"
        Just toTarget -> do
          putStrLn "How much?"
          _amount <- getLine
          let amount = readMaybe _amount :: Maybe Float
          case amount of
            Nothing -> pure $ Left "Could not parse that as a number"
            Just a -> pure $ Right $ doInterBucketTransaction root (fromTarget, toTarget) a

deleteUI :: Bucket -> IO (Either String Bucket)
deleteUI root = do
  _target <- selectBucketUI root "Which bucket name should be deleted?"
  case _target of
    Nothing -> pure $ Left "Could not find that bucket"
    Just target -> pure $ Right $ deleteBucket root (== target)

updateUI :: Bucket -> IO (Either String Bucket)
updateUI root = do
  _target <- selectBucketUI root "Which bucket name should be updated?"
  case _target of
    Nothing -> pure $ Left "Could not find bucket"
    Just target -> do
      newName <- collectBucketNameUI
      newPortion <- collectBucketPortionUI
      pure $ updateSubBucket root (== target) (\b -> b & bucketName .~ newName & bucketPortion .~ newPortion)
