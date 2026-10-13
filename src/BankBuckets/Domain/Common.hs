module BankBuckets.Domain.Common  (DomainObject(..)) where

class DomainObject obj where
  idOf :: obj -> Int
