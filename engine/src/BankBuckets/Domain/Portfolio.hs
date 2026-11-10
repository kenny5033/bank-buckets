{-# LANGUAGE TemplateHaskell #-}

module BankBuckets.Domain.Portfolio
  ( Portfolio (..),
    PortfolioRepo (..),
    portfolioId,
    portfolioRootBucket,
    newPortfolio,
  )
where

import BankBuckets.Domain.Bucket
import Control.Lens

data Portfolio = Portfolio
  { _portfolioId :: Maybe Int,
    _portfolioRootBucket :: Bucket
  }

makeLenses ''Portfolio

newPortfolio :: String -> Portfolio
newPortfolio name =
  Portfolio
    { _portfolioId = Nothing,
      _portfolioRootBucket =
        Bucket
          { _bucketName = name,
            _bucketAmount = 0,
            _bucketPortion = 100,
            _bucketSubBuckets = []
          }
    }

class PortfolioRepo repo where
  upsertPortfolio :: repo -> Portfolio -> IO Portfolio
  readPortfolio :: repo -> Int -> IO (Maybe Portfolio)
  deletePortfolio :: repo -> Int -> IO ()
