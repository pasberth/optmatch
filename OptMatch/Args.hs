module OptMatch.Args where

import Control.Applicative
import Control.Monad.State
import OptMatch.Matcher

type Args = [String]

keyword :: Monad m => String -> MatcherT Args m String
keyword s = do { x <- shift; expect s (return x) }

argument :: Monad m => MatcherT Args m String
argument = shift

anywhere :: Monad m => MatcherT Args m a -> MatcherT Args m a
anywhere m = m <|> do
  args <- get
  case args of
    [] -> mzero
    (x:xs) -> do
      put xs
      a <- anywhere m
      xs <- get
      put (x:xs)
      return a
