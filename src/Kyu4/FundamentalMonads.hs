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
  return = undefined
  (Identity v) >>= f = undefined

instance Monad Maybe where
  return = undefined
  Nothing >>= f = undefined
  (Just v) >>= f = undefined

instance Monad (State s) where
  return = undefined
  (State g) >>= f = undefined

instance Monad (Reader s) where
  return = undefined
  (Reader g) >>= f = undefined

instance Monoid w => Monad (Writer w) where
  return = undefined
  (Writer (s, v)) >>= f = undefined
