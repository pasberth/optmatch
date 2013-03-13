{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module System.Console.OptMatch.Types where

type Args = [String]

class ArgType a where
  toArg :: a -> String

instance ArgType String where
  toArg = id

instance ArgType Integer where
  toArg = show
