{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Kyu2.Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance Succ m :< Succ n = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero n = n 
type instance Add (Succ m) n = Succ (Add m n) 

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min Zero n = Zero
type instance Min n Zero = Zero
type instance Min (Succ n) (Succ m)= Succ (Min n m)

type family (Minus (a :: Nat) (b :: Nat)) :: Nat
type instance Minus Zero n = Zero
type instance Minus n Zero = n
type instance Minus (Succ n) (Succ m)= Minus n m

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x xs) = x 
index (SSucc m) (VCons x xs) = index m xs

replicate :: s -> SNat a -> Vec s a
replicate x SZero = VNil
replicate x (SSucc m) = VCons x $ replicate x m

-- Both vectors must be of equal length
-- zipWith :: (a :< b ~ True, b :< a ~ True) => (x -> y -> z) -> Vec x a -> Vec y b -> Vec z a
zipWith :: (x -> y -> z) -> Vec x a -> Vec y a -> Vec z a
zipWith f VNil VNil  = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) $ zipWith f xs ys 

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ b = b
VCons x xs ++ b = VCons x $ xs ++ b

-- The semantics should match that of take for normal lists.
take :: SNat a -> Vec x b -> Vec x (Min a b) 
take SZero b = VNil 
take _ VNil = VNil
take (SSucc a) (VCons x xs) = VCons x $ take a xs

-- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec x b -> Vec x (Minus b a)
drop SZero b = b
drop (SSucc a) VNil = VNil
drop (SSucc a) (VCons x xs) = drop a xs

head :: Vec x a -> x
head (VCons x xs) = x

tail :: Vec x (Succ a) -> Vec x a
tail (VCons x xs) = xs

