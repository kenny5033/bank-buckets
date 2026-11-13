module BankBuckets.Interface.Common (openDomainSocket, closeSocket) where

import BankBuckets.Common
import Network.Socket
import System.Directory
import System.FilePath

openDomainSocket :: String -> IO Socket
openDomainSocket socketName = do
  sock <- socket AF_UNIX Stream defaultProtocol
  dir <- appRuntimeDir
  let path = dir </> socketName
  removePathForcibly path
  bind sock (SockAddrUnix $ dir </> socketName)
  listen sock maxListenQueue
  pure sock

closeSocket :: Socket -> IO ()
closeSocket sock = do
  path <- getSocketName sock
  close sock
  case path of
    SockAddrUnix p -> removePathForcibly p
    _ -> pure ()
