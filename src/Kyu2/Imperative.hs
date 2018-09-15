{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Kyu2.Imperative where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad

newtype SomeMonad a = SomeMonad (State (Integer, M.Map Integer Integer) a) deriving (Applicative, Monad)
data SomeVariable = SomeVariable {getInt :: Integer} | Constance Integer

instance Functor SomeMonad where
  fmap f (SomeMonad st)= SomeMonad (fmap f st)

makeST = SomeMonad . state
runST (SomeMonad m) = runState m
evalST (SomeMonad m) = evalState m
execST (SomeMonad m) = execState m

def :: SomeMonad SomeVariable -> Integer
def m = case runST m (0, M.empty) of 
          (SomeVariable k, (_, mp)) -> mp M.! k
          (Constance k, _) -> k

var :: Integer -> SomeMonad SomeVariable
var v = makeST (\(k, mp) -> (SomeVariable k, (k+1, M.insert k v mp)))
lit :: Integer -> SomeVariable
lit = Constance

while :: SomeVariable -> (Integer -> Bool) -> SomeMonad () -> SomeMonad ()
while sv f m = makeST (\(k, mp) -> if f $ mp M.! getInt sv 
                                      then ((), execST (while sv f m) (execST m (k, mp))) 
                                      else ((), (k, mp)))

getValue (Constance c) mp = c
getValue (SomeVariable c) mp = mp M.! c

(+=), (-=), (*=) :: SomeVariable -> SomeVariable -> SomeMonad ()
(SomeVariable k) += y = makeST (\(m, mp) -> ((), (m, M.insert k ((mp M.! k) + getValue y mp) mp)))
_ += _ = error "wrong parametor"

(SomeVariable k) -= y = makeST (\(m, mp) -> ((), (m, M.insert k ((mp M.! k) - getValue y mp) mp)))
_ -= _ = error "wrong parametor"

(SomeVariable k) *= y = makeST (\(m, mp) -> ((), (m, M.insert k ((mp M.! k) * getValue y mp) mp)))
_ *= _ = error "wrong parametor"

factorial :: Integer -> Integer
factorial n = def $ do
  result <- var 1
  i      <- var n
  while i (>0) $ do
    result *= i
    i      -= lit 1
  return result

com = def $ do
  a <- var 1
  b <- var 2
  a += b
  a += lit 1
  return a 
-- should be 4
main = print $ factorial 5
