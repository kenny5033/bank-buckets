{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Interface.JSONRPC (runJSONRPC) where

import BankBuckets.Domain.Portfolio
import BankBuckets.Interface.Common
import Data.Aeson
import GHC.Generics
import GHC.IO
import JSONRPC
import Network.HTTP.Types
import Network.Wai
import Network.Wai qualified as Wai
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
jsonRPCApp req respond
  | Wai.requestMethod req /= methodPost = respond $ responseLBS status405 [] "POST only"
  | otherwise = do
      body <- strictRequestBody req
      case eitherDecode body :: Either String Value of
        Left _ -> respond $ jsonResp (failure (-32700, "Parse error"))
        Right v -> case fromJSON v of
          Success PingRequest {id = rid, params = ps} ->
            respond $ jsonResp (success rid $ object ["pong" .= ps])
          _ ->
            respond $ jsonResp (failure (-32600, "Invalid request"))

jsonResp :: Value -> Response
jsonResp v = responseLBS status200 [(hContentType, "application/json")] $ encode v

success :: RequestId -> Value -> Value
success rid result =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "id" .= rid,
      "result" .= result
    ]

failure :: (Int, String) -> Value
failure (code, msg) =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "id" .= Null,
      "error" .= object ["code" .= code, "message" .= msg]
    ]

data PingRequest = PingRequest
  { id :: RequestId,
    params :: Maybe [Int]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via ViaJSONRPCRequest PingRequest

instance IsJSONRPCRequest PingRequest where
  requestMethod _ = "ping"
