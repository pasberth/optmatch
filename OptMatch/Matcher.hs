module OptMatch.Matcher where

import Control.Monad
import Control.Monad.Trans

data Reply s a = Continue a s
               | Failed

newtype MatcherT s m a = MatcherT {
  runMatcherT :: s -> m (Reply s a)
  }

instance Monad m => Monad (MatcherT s m) where
  return a = MatcherT $ \s -> return (Continue a s)
  m >>= k = MatcherT $ \s -> do
    reply <- runMatcherT m s
    case reply of
      Continue a s' -> runMatcherT (k a) s'
      Failed -> return Failed

instance MonadTrans (MatcherT s) where
  lift m = MatcherT $ \s -> do
    a <- m
    return $ Continue a s

optional :: Monad m => MatcherT s m a -> MatcherT s m (Maybe a)
optional m = MatcherT $ \s -> do
  reply <- runMatcherT m s
  case reply of
    Continue a _ -> return $ Continue (Just a) s
    Failed -> return $ Continue Nothing s
