module BankBuckets (run, RunMode (..)) where

import BankBuckets.Common
import BankBuckets.Interface.CLI
import BankBuckets.Interface.JSONRPC
import BankBuckets.Storage.SQLite
import Database.SQLite.Simple (open)
import System.FilePath

data RunMode = CLI | JSONRPC

run :: RunMode -> IO ()
run mode = do
  dataDir <- appDataDir

  conn <- open (if isDevelopment then ":memory:" else dataDir </> "bb.db")
  portfolioRepo <- newPortfolioSQLiteRepo conn

  case mode of
    CLI -> runCLI portfolioRepo
    JSONRPC -> runJSONRPC portfolioRepo
