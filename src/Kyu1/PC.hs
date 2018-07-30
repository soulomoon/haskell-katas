{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module Kyu1.PC where

import Data.List

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (a3b3, b3a3) 
    where a3b3 a3 = (ab .) . (. ba) . (. ba) a3
          b3a3 b3 = (ba .) . (. ab) . (. ab) b3

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
    zero = O
    successor = S
    nat a _ O = a
    nat a f (S n) = f n
    iter a _ O = a
    iter a f (S x) = iter (f a) f x
    plus x = iter x successor
    minus x = iter x predecessor
        where predecessor (S n) = n
              predecessor O = O
    mult x = iter zero (plus x)
    pow x = iter (successor zero) (mult x)

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
    zero = []
    successor x = () : x
    nat a f [] = a
    nat a f (():xs) = f xs
    iter a _ [] = a
    iter a f (():xs) = iter (f a) f xs
    plus x = iter x successor
    minus x = iter x predecessor
        where predecessor (():xs) = xs
              predecessor [] = []
    mult x = iter zero (plus x)
    pow x = iter (successor zero) (mult x)
    
-- Instead of defining Nat from zero, successor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

  zero = Scott const
  successor v = Scott (\ a f -> f v) 

  iter i g (Scott a) = a i (iter (g i) g)
  nat i f (Scott a) = a i f 
  


-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.
  zero = Church (\ _ z -> z)
  successor (Church a) = Church (\ s z -> s (a s z))
  iter i f (Church a) = a f i
  plus (Church a) (Church b) = Church (\ s z -> (b s (a s z)))
  minus = substR (liftISO2 isoP) minus
  nat a f (Church n) =  if n (+1) 0 == 0 
                           then a 
                           else f $ Church n `minus` successor zero
  mult (Church a) (Church b) = Church (a . b)
  pow (Church a) (Church b) = Church (b a)
