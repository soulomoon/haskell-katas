{-# LANGUAGE RankNTypes #-}

module Kyu4.ChurchNumbers where

import Prelude hiding (succ, pred, fst, snd)

pred :: Number -> Number
pred n = fst $ head $ filter ((== eval n) . eval . snd) $ pair zero zero : map (\ x -> pair x $ succ x) (iterate succ zero)

subt :: Number -> Number -> Number
subt b (Nr a) = a pred b

-- cannot be placed in Preloaded right now as there is a bug in the codewars system

newtype Pair a b = Pr (forall c . (a -> b -> c) -> c)

instance (Show a, Show b) => Show (Pair a b) where
  show (Pr p) = p (\ a b -> "(" ++ show a ++ "," ++ show b ++ ")")

pair :: a -> b -> Pair a b
pair f s = Pr (\ b -> b f s)

fst :: Pair a b -> a
fst (Pr p) = p (\ a _ -> a)

snd :: Pair a b -> b
snd (Pr p) = p (\ _ b -> b)

newtype Number = Nr (forall a. (a -> a) -> a -> a)

instance Show Number where
  show (Nr a) = a ("1+" ++) "0"
  
instance Eq Number where
  a == b = eval a == eval b
  
fold :: Number -> (a -> a) -> a -> a
fold (Nr n) s z = n s z

eval :: Number -> Integer
eval (Nr a) = a (+1) 0

zero :: Number
zero = Nr (\ _ z -> z)

succ :: Number -> Number
succ (Nr a) = Nr (\ s z -> s (a s z))

add :: Number -> Number -> Number
add (Nr a) = a succ

mult :: Number -> Number -> Number
mult (Nr a) b =  a (add b) zero
