module OptMatch.Args where

import OptMatch.Matcher

type Args = [String]

keyword :: Monad m => String -> MatcherT Args m String
keyword s = MatcherT $ \args -> case args of
  (x:xs)
    | x == s -> return $ Continue x xs
    | otherwise -> return Failed
  otherwise -> return Failed

argument :: Monad m => MatcherT Args m String
argument = MatcherT $ \args -> case args of
  (x:xs) -> return $ Continue x xs
  otherwise -> return Failed

switch :: Monad m => String -> a -> a -> MatcherT Args m a
switch s x y = do
  a <- optional $ keyword s
  case a of
    Just _ -> return x
    Nothing -> return y

opton :: Monad m => String -> MatcherT Args m Bool
opton s = switch s True False

optoff :: Monad m => String -> MatcherT Args m Bool
optoff s = switch s False True
