{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module Kyu1.TimesComm where

data Z
data S n

data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

(<+>) :: Natural a -> Natural b -> Natural (a :+: b)
(<+>) NumZ b = b
(<+>) (NumS a) b = NumS $ a <+> b

(<.>) :: Natural a -> Natural b -> Natural (a :*: b)
(<.>) NumZ b = NumZ
(<.>) (NumS a) b = b <+> (a <.> b)

(|=|) :: Natural a -> Natural b -> Equal a b
(|=|) NumZ NumZ = EqlZ
(|=|) (NumS a) (NumS b) = EqlS $ a |=| b

infixr 3 <==
(<==) :: Equal a b -> Equal b c -> Equal a c
(<==) EqlZ  EqlZ = EqlZ
(<==) (EqlS ab) (EqlS bc) = EqlS $ ab <== bc

(+++) :: Equal n m -> Equal a b -> Equal (n :+: a) (m :+: b)
(+++) EqlZ ab = ab
(+++) (EqlS nm) ab = EqlS $ nm +++ ab

reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS x) = EqlS $ reflexive x

symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS eq) = EqlS $ symmetric eq

plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ NumZ a = reflexive a
plusAssoc NumZ (NumS b) c= EqlS $ plusAssoc NumZ b c
plusAssoc (NumS a) b c= EqlS $ plusAssoc a b c

plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm NumZ NumZ = EqlZ
plusComm (NumS x) NumZ = EqlS $ plusComm x NumZ
plusComm NumZ x = symmetric $ plusComm x NumZ
plusComm as@(NumS a) bs@(NumS b) =
  EqlS $ plusComm a bs
  <== EqlS (plusComm b a)
  <== plusComm as b

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ = EqlZ
zeroComm (NumS a) = zeroComm a

timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ x = zeroComm x
timesComm x NumZ = symmetric $ zeroComm x
timesComm as@(NumS a) bs@(NumS b) =
  -- as * bs == bs + (a * bs)
  -- bs + (a * bs) = 1 + (b + (a * bs))
  EqlS $
  -- b + (a * bs) == b + (bs * a)
  reflexive b +++ timesComm a bs
  -- b + (bs * a) == b + (a + (b * a))
  -- b + (a + (b * a)) == (b + a) + (b * a)
  <== plusAssoc b a (b <.> a)
  -- (b + a) + (b * a) == (a + b) + (a * b)
  <== plusComm b a +++ timesComm b a
  -- (a + b) + (a * b)  == a + (b + (a * b))
  <== symmetric (plusAssoc a b (a <.> b))
  -- a + (b + (a * b)) == a + (as * b)
  -- a + (as * b) == a + (b * as)
  <== reflexive a +++ timesComm as b


  -- mul comm :
  --   as * bs
  --   == bs + (a * bs) // mul -> add
  --   == 1 + (b + (a * bs))  // add -> succ
  --   (1+) gap // strip succ
  --   == (b + (a * bs)) //
  --   == (b + (bs * a)) // mul comm
  --   == (b + (a + (b * a))) // mul -> add
  --   == (b + a) + (b * a) // add assoc
  --   == (a + b) + (a * b) // add comm and mul comm
  --   == a + (b + (a * b)) // add assoc
  --   == a + (as * b) //  add -> mul
  --   == a + (b * as) // mul comm
  --   (1+) gap // compansate succ
  --   == 1 + (a + (b * as))
  --   == (as + (b * as)) // succ -> add
  --   bs * as //add

  --
  --
