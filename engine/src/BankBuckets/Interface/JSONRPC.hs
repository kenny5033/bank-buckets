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
runJSONRPC portfolioRepo =
  bracket
    (openDomainSocket "jsonrpc.sock")
    closeSocket
    ( \sock -> do
        runSettingsSocket defaultSettings sock $ jsonRPCApp portfolioRepo
    )

jsonRPCApp :: (PortfolioRepo r) => r -> Application
jsonRPCApp repo req respond
  | Wai.requestMethod req /= methodPost = respond $ responseLBS status405 [] "POST only"
  | otherwise = do
      body <- strictRequestBody req
      case eitherDecode body :: Either String (JSONRPCRequest Value) of
        Left err -> jsonRPCFailure (-32700, "Parse error when parsing request", Just err)
        Right JSONRPCRequest {method, id = rid, params} -> case method of
          "ping" -> do
            let maybeResp = (\r -> object ["pong" .= r]) <$> (fromJSON params :: Result PingRequest)
            case maybeResp of
              Success r -> jsonRPCSuccess rid r
              _ -> jsonRPCFailure (-32700, "Parse error when parsing params", Just params)
          "addBucket" -> do
            let maybeReq = rpcAddBucket repo <$> (fromJSON params :: Result AddBucketRequest)
            case maybeReq of
              Success r -> r >>= jsonRPCSuccess rid
              _ -> jsonRPCFailure (-32700, "Parse error when parsing params", Just params)
          _ -> jsonRPCFailure (-32600, "Invalid method", Nothing :: Maybe String)
  where
    jsonRPCSuccess :: (ToJSON r) => JSONRPCID -> r -> IO ResponseReceived
    jsonRPCSuccess id' = respond . jsonResp . toJSON . newJSONRPCSuccessResponse id'

    jsonRPCFailure :: (ToJSON r) => (Int, String, Maybe r) -> IO ResponseReceived
    jsonRPCFailure = respond . jsonResp . toJSON . newJSONRPCFailureResponse

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

newJSONRPCSuccessResponse :: (ToJSON r) => JSONRPCID -> r -> JSONRPCResponse r
newJSONRPCSuccessResponse rid r =
  JSONRPCResponse
    { jsonrpc = "2.0",
      result = Just r,
      error = Nothing,
      id = rid
    }

newJSONRPCFailureResponse :: (ToJSON r) => (Int, String, Maybe r) -> JSONRPCResponse r
newJSONRPCFailureResponse (code, msg, r) =
  JSONRPCResponse
    { jsonrpc = "2.0",
      result =
        Nothing,
      error = Just JSONRPCErrorResponse {code = code, message = msg, data' = r},
      id = Nothing
    }

jsonResp :: Value -> Response
jsonResp v = responseLBS status200 [(hContentType, "application/json")] $ encode v
