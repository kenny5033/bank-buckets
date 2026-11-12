{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Interface.JSONRPC (runJSONRPC) where

import BankBuckets.Domain.Portfolio
import BankBuckets.Interface.Common
import GHC.IO
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

runJSONRPC :: (PortfolioRepo r) => r -> IO ()
runJSONRPC _portfolioRepo =
  bracket
    (openDomainSocket "jsonrpc.sock")
    closeDomainSocket
    ( \sock -> do
        runSettingsSocket defaultSettings sock jsonRPCApp
    )

jsonRPCApp :: Application
jsonRPCApp req respond = respond $ responseLBS status200 [("Content-Type", "application/json")] "{\"test\": true}"
