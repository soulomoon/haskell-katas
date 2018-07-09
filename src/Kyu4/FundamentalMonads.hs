module Kyu4.FundamentalMonads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State g) >>= f = State $ \s -> let (State ng) = f a 
                                      (a, ns) = (g s)
                                  in ng ns

instance Monad (Reader s) where
  return x = Reader $ \s -> x
  (Reader g) >>= f = Reader $ \s -> let (Reader ng) = f (g s)
                                    in ng s

instance Monoid w => Monad (Writer w) where
  return a = Writer (mempty, a)
  (Writer (s, v)) >>= f = let Writer (ns, nv) = f v 
                          in Writer (mappend ns s, nv)
