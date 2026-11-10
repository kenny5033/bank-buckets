{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Storage.SQLite (newPortfolioSQLiteRepo) where

import BankBuckets.Domain.Bucket
import BankBuckets.Domain.Portfolio
import Control.Lens
import Data.Aeson
import Data.ByteString
import Data.Text
import Data.Text.Encoding
import Database.SQLite.Simple

newtype PortfolioSQLiteRepo = PortfolioSQLiteRepo Connection

bucketToRowData :: Bucket -> Text
bucketToRowData = decodeUtf8 . toStrict . encode

rowDataToBucket :: Text -> Maybe Bucket
rowDataToBucket = decode . fromStrict . encodeUtf8

newPortfolioSQLiteRepo :: Connection -> IO PortfolioSQLiteRepo
newPortfolioSQLiteRepo conn = do
  execute_
    conn
    """
    CREATE TABLE IF NOT EXISTS portfolios
    (
      id INTEGER PRIMARY KEY,
      root_bucket TEXT NOT NULL DEFAULT '{}' CHECK (json_valid(root_bucket))
    );
    """
  pure $ PortfolioSQLiteRepo conn

instance PortfolioRepo PortfolioSQLiteRepo where
  upsertPortfolio
    (PortfolioSQLiteRepo conn)
    portfolio = case portfolio ^. portfolioId of
      Nothing -> do
        res <-
          query
            conn
            "INSERT INTO portfolios (root_bucket) VALUES (?) RETURNING id"
            (Only $ bucketToRowData $ portfolio ^. portfolioRootBucket) ::
            IO [Only Int]
        case res of
          [] -> pure portfolio
          x : _ -> pure $ portfolio & portfolioId ?~ fromOnly x
      Just id' -> do
        execute
          conn
          "UPDATE portfolios SET root_bucket = ? WHERE id = ?"
          (bucketToRowData $ portfolio ^. portfolioRootBucket, id')
        pure portfolio

  readPortfolio (PortfolioSQLiteRepo conn) dbId = do
    res <-
      query
        conn
        "SELECT root_bucket FROM portfolios WHERE id = ?"
        (Only dbId) ::
        IO [Only Text]
    case res of
      [] -> pure Nothing
      x : _ -> do
        let loadedBucket = rowDataToBucket $ fromOnly x
        case loadedBucket of
          Nothing -> pure Nothing
          Just b -> pure $ pure Portfolio {_portfolioRootBucket = b, _portfolioId = Just dbId}

  deletePortfolio (PortfolioSQLiteRepo conn) dbId = do
    execute
      conn
      "DELETE FROM portfolios WHERE id = ?"
      (Only dbId)
