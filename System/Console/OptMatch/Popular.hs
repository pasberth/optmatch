module System.Console.OptMatch.Popular where

import Control.Applicative
import Control.Monad
import System.Console.OptMatch
import System.Console.OptMatch.Prim

popular :: (Functor m, Monad m) => a -> (a -> OptMatchT m a) -> OptMatchT m a
popular a m = do
  r <- m a <|> fixAndRetry
  popular r m <|> return r where
    fixAndRetry = do
      optional expandShortOptions
      m a

expandShortOptions :: (Functor m, Monad m) => OptMatchT m ()
expandShortOptions = do
  unexpect $ prefix "--"
  prefix "-"
  str <- shift
  forM (reverse str) $ \c -> unshift ['-', c]
  return ()
