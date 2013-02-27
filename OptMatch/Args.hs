module OptMatch.Args where

import qualified Data.List as L
import OptMatch.Matcher

type Args = [String]

keyword :: Monad m => String -> MatcherT Args m String
keyword s = MatcherT $ \args -> case args of
  (x:xs)
    | x == s -> return $ Continue x xs
    | otherwise -> return Failed
  otherwise -> return Failed

prefix :: Monad m => String -> MatcherT Args m String
prefix s = MatcherT $ \args -> case args of
  (x:xs)
    | L.isPrefixOf s x -> let Just x' = L.stripPrefix s x in
      return $ Continue s (x':xs)
    | otherwise -> return Failed
  otherwise -> return Failed

argument :: Monad m => MatcherT Args m String
argument = MatcherT $ \args -> case args of
  (x:xs) -> return $ Continue x xs
  otherwise -> return Failed
