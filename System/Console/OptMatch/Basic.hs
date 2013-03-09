{-# LANGUAGE FlexibleContexts #-}
module System.Console.OptMatch.Basic where

import Control.Applicative
import Control.Monad.State
import System.Console.OptMatch
import System.Console.OptMatch.Types
import System.Console.OptMatch.Prim

keyword :: (Eq a, MonadState [a] m, MonadPlus m) => a -> m a
keyword = just

argument :: (MonadState [a] m, MonadPlus m) => m a
argument = shift

subst :: (MonadState Args m, MonadPlus m) => String -> m String
subst = prefix . (++"=")

basic :: (Functor m, Monad m) => a -> (a -> OptMatchT m a) -> OptMatchT m a
basic def m = f def where
  f a = do
    r <- m a
    basic r m <|> return r
