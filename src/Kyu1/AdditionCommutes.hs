{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kyu1.AdditionCommutes
  ( plusCommutes ) where

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS $ reflexive n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS eq) = EqlS $ symmetric eq

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ 
transitive (EqlS eq1) (EqlS eq2) = EqlS $ transitive eq1 eq2

oppositeSide :: Natural a -> Natural b -> Equal (S (a :+: b)) (a :+: S b)
oppositeSide NumZ b = EqlS $ reflexive b
oppositeSide (NumS a) b = EqlS $ oppositeSide a b

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes NumZ (NumS a) = EqlS $ plusCommutes NumZ a
plusCommutes (NumS a) bs = transitive (EqlS (plusCommutes a bs)) (oppositeSide bs a)
