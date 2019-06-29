{-# LANGUAGE RankNTypes #-}

module Kyu2.Tagless where

import Prelude hiding (and, or)

class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)
  
  loop   :: r h (a -> a) -> r h a
  
  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool -- greater than or equal
  
  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool
  
  ifte   :: r h Bool -> r h a -> r h a -> r h a -- if true then return left term, else return right term

type Term a = forall r h . Language r => r h a
  
instance Language (->) where
    here = fst
    before = (. snd)
    lambda = flip . curry
    apply ff fa h = ff h $ fa h

    loop ff h = ff h $ loop ff h

    int = const
    add f g h = f h + g h
    down f h = f h - 1
    up f h = f h + 1
    mult f g h = f h * g h
    gte f g h = f h >= g h

    bool = const
    and f g h = f h && g h
    or f g h = f h || g h
    neg = (not .)

    ifte test l r h = if test h then l h else r h




interpret :: Term a -> a
interpret t = t ()
