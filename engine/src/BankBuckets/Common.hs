module BankBuckets.Common (version, isDevelopment, appDataDir, appRuntimeDir) where

import System.Directory

version :: String
version = "1.0"

isDevelopment :: Bool
isDevelopment = True

appDataDir :: IO FilePath
appDataDir = do
  d <- getXdgDirectory XdgData "bankbuckets"
  createDirectoryIfMissing True d
  pure d

appRuntimeDir :: IO FilePath
appRuntimeDir = do
  d <- getXdgDirectory XdgCache "bankbuckets"
  createDirectoryIfMissing True d
  pure d
