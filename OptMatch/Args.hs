module OptMatch.Args where

import qualified Data.List as L
import OptMatch.Matcher
import Control.Monad.State

type Args = [String]

keyword :: Monad m => String -> MatcherT Args m String
keyword s = do
  args <- get
  case args of
    [] -> mzero
    (x:xs)
      | x == s -> do { put xs; return x }
      | otherwise -> mzero

prefix :: Monad m => String -> MatcherT Args m String
prefix s = do
  args <- get
  case args of
    [] -> mzero
    (x:xs)
      | L.isPrefixOf s x -> let Just x' = L.stripPrefix s x in
        do { put (x':xs); return x }
      | otherwise -> mzero

argument :: Monad m => MatcherT Args m String
argument = do
  args <- get
  case args of
    [] -> mzero
    (x:xs) -> do { put xs; return x }
