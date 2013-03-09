module System.Console.OptMatch.Popular where

import Control.Applicative
import System.Console.OptMatch

popular :: (Functor m, Monad m) => a -> (a -> OptMatchT m a) -> OptMatchT m a
popular def m = f def where
  f a = do
    r <- m a
    popular r m <|> return r
