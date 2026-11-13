{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module BankBuckets.Interface.JSONRPC (runJSONRPC) where

import BankBuckets.Domain.Portfolio
import BankBuckets.Interface.Common
import BankBuckets.Interface.RPCSpec
import Data.Aeson
import GHC.Generics
import GHC.IO
import Network.HTTP.Types
import Network.Wai
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp

runJSONRPC :: (PortfolioRepo r) => r -> IO ()
runJSONRPC _portfolioRepo =
  bracket
    (openDomainSocket "jsonrpc.sock")
    closeSocket
    ( \sock -> do
        runSettingsSocket defaultSettings sock jsonRPCApp
    )

jsonRPCApp :: Application
jsonRPCApp req respond
  | Wai.requestMethod req /= methodPost = respond $ responseLBS status405 [] "POST only"
  | otherwise = do
      body <- strictRequestBody req
      case eitherDecode body :: Either String (JSONRPCRequest Value) of
        Left _ -> jsonRPCFailure (-32700, "Parse error when parsing request")
        Right JSONRPCRequest {method, id = rid, params} -> case method of
          "ping" -> do
            let maybeReq = fromJSON params :: Result PingRequest
            case maybeReq of
              Success r -> jsonRPCSuccess rid $ object ["pong" .= r]
              _ -> jsonRPCFailure (-32700, "Parse error when parsing params")
          "addBucket" -> do
            let maybeReq = fromJSON params :: Result AddBucketRequest
            case maybeReq of
              Success r -> jsonRPCSuccess rid r
              _ -> jsonRPCFailure (-32700, "Parse error when parsing params")
          _ -> jsonRPCFailure (-32600, "Invalid method")
  where
    jsonRPCSuccess :: (ToJSON r) => JSONRPCID -> r -> IO ResponseReceived
    jsonRPCSuccess id' r =
      respond . jsonResp . toJSON $
        JSONRPCResponse
          { jsonrpc = "2.0",
            result = Just r,
            error = Nothing,
            id = id'
          }

    jsonRPCFailure :: (Int, String) -> IO ResponseReceived
    jsonRPCFailure (code, msg) =
      respond . jsonResp . toJSON $
        ( JSONRPCResponse
            { jsonrpc = "2.0",
              result =
                Nothing,
              error = Just JSONRPCErrorResponse {code = code, message = msg, data' = Nothing},
              id = Nothing
            } ::
            JSONRPCResponse ()
        )

type JSONRPCID = Maybe Integer

data JSONRPCRequest r = JSONRPCRequest
  { jsonrpc :: String,
    method :: String,
    params :: r,
    id :: JSONRPCID
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data JSONRPCErrorResponse d = JSONRPCErrorResponse
  { code :: Int,
    message :: String,
    data' :: Maybe d
  }
  deriving (Show, Eq, Generic)

instance (ToJSON d) => ToJSON (JSONRPCErrorResponse d) where
  toJSON r = object ["code" .= r.code, "message" .= r.message, "data" .= r.data']

instance (FromJSON d) => FromJSON (JSONRPCErrorResponse d) where
  parseJSON =
    withObject "JSONRPCErrorResponse" $ \v ->
      JSONRPCErrorResponse <$> v .: "code" <*> v .: "message" <*> v .:? "data"

data JSONRPCResponse r = JSONRPCResponse
  { jsonrpc :: String,
    result :: Maybe r,
    error :: Maybe (JSONRPCErrorResponse r),
    id :: JSONRPCID
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

jsonResp :: Value -> Response
jsonResp v = responseLBS status200 [(hContentType, "application/json")] $ encode v
