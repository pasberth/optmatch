module OptMatch.Matcher where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

data Reply s a = Continue a s
               | Failed

newtype MatcherT s m a = MatcherT {
  runMatcherT :: s -> m (Reply s a)
  }

instance Functor (Reply s) where
  fmap f (Continue a s) = Continue (f a) s
  fmap f Failed = Failed

instance Monad m => Functor (MatcherT s m) where
  fmap f m = MatcherT $ \s -> liftM (fmap f) (runMatcherT m s)

instance Monad m => Applicative (MatcherT s m) where
  pure = return
  (<*>) = ap

instance Monad m => Alternative (MatcherT s m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => Monad (MatcherT s m) where
  return a = MatcherT $ \s -> return (Continue a s)
  m >>= k = MatcherT $ \s -> do
    reply <- runMatcherT m s
    case reply of
      Continue a s' -> runMatcherT (k a) s'
      Failed -> return Failed

instance Monad m => MonadPlus (MatcherT s m) where
  mzero = MatcherT $ \s -> return Failed
  mplus m k = MatcherT $ \s -> do
    reply <- runMatcherT m s
    case reply of
      Failed -> runMatcherT k s
      otherwise -> return reply

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
