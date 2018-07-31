module Kyu3.ChurchNumber where

type Lambda a = (a -> a)
type Cnum a = Lambda a -> Lambda a

zero :: Cnum a
zero f = id

churchify 0 = zero
churchify n = churchSucc (churchify (n-1))

numerify :: Cnum Int -> Int
numerify c = c (+1) 0

churchSucc :: Cnum a -> Cnum a
churchSucc c h = h . c h

churchAdd :: Cnum a -> Cnum a -> Cnum a
churchAdd c1 c2 s z = c1 s (c2 s z)

churchMul :: Cnum a -> Cnum a -> Cnum a
churchMul c1 c2 = c1 . c2

--Extra credit: Why is the type signature different?
churchPow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
churchPow cb ce = ce cb
