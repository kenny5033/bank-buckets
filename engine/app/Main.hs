module Main (main) where

import BankBuckets
import System.Environment
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    ["cli"] -> BankBuckets.run CLI
    ["jsonrpc"] -> BankBuckets.run JSONRPC
    _ -> printf "Usage is: %s <run-mode: cli, jsonrpc>\n" progName
