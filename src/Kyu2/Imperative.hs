module Kyu2.Imperative (
) where

-- import Control.Monad

-- data SomeMonad a  = SomeMonad a

-- instance Applicative SomeMonad where
--     pure = return 
--     (<*>) = ap

-- instance Monad SomeMonad where
--     return = SomeMonad
--     (SomeMonad a) >>= f = f a

-- def :: SomeMonad Integer -> Integer
-- def (SomeMonad n) = n
-- var :: Integer -> SomeMonad Integer
-- var = return
-- lit :: Integer -> SomeMonad Integer
-- lit = var

-- while :: SomeMonad Integer -> (Integer -> Bool) -> SomeMonad () -> SomeMonad ()
-- while r f act = do
--     let c = f r
--     when c $ act >> while r f act 

-- (+=), (-=), (*=) ::SomeMonad Integer -> Integer -> SomeMonad ()
-- a += b = var $ a + b
-- a -= b = var $ a - b
-- a *= b = var $ a * b

-- main = do
--     a <- var 1
--     while a (>0) $ do 
--         a -= 1
--         return ()
--     return a

