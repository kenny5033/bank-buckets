module BankBuckets.Interface.JSONRPC (runJSONRPC) where

import BankBuckets.Domain.Portfolio

runJSONRPC :: (PortfolioRepo r) => r -> IO ()
runJSONRPC _portfolioRepo = do
  putStrLn "WIP :)"
  pure ()
