module BankBuckets.Common (DomainRepo (..), InMemoryRepo (..), DomainObject (..)) where

import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Map qualified as Map

class DomainObject obj where
  idOf :: obj -> Int

class DomainRepo repo where
  add :: (DomainObject a) => repo a -> a -> IO ()
  update :: (DomainObject a) => repo a -> a -> IO ()
  load :: (DomainObject a) => repo a -> Int -> IO (Maybe a)
  delete :: (DomainObject a) => repo a -> Int -> IO ()

newtype InMemoryRepo a = InMemoryRepo (IORef (Map.Map Int a))

instance DomainRepo InMemoryRepo where
  add (InMemoryRepo ref) x = modifyIORef' ref (Map.insert (idOf x) x)
  update (InMemoryRepo ref) x = modifyIORef' ref (Map.insert (idOf x) x)
  load (InMemoryRepo ref) i = Map.lookup i <$> (readIORef ref)
  delete (InMemoryRepo ref) i = modifyIORef' ref (Map.delete i)
