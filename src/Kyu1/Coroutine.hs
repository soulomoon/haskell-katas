{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Kyu1.Coroutine where
import Control.Monad (ap, forever, (>=>))
import Data.List


newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)

data Command r u d a =
       Done a
     | Out d (Coroutine r u d a)
     | In (u -> Coroutine r u d a) deriving Functor


-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return x = Coroutine ($ Done x)
  f >>= g = Coroutine 
    (\k -> apply f 
      (\case 
        Done x -> apply (g x) k 
        Out d newf -> k $ Out d (newf >>= g)
        In ff -> k (In $ ff >=> g)))

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
x >>> y = Coroutine 
    (\k -> apply y 
        (\case
          Done e -> k (Done e)
          Out d newy -> k $ Out d (x >>> newy)
          In my -> apply x 
                      (\case 
                        Done e -> k (Done e)
                        Out m newx -> apply (newx >>> my m) k
                        In ux -> k $ In $ \u -> ux u >>> y)))

-- It might be useful to define the following function
-- pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a

-- Library functions
done :: a -> Coroutine r u d a
done a = Coroutine ($ Done a)

coIn f = Coroutine ($ In f)  

coOut x next = Coroutine ($ Out x next)  

output :: a -> Coroutine r u a ()
output v = coOut v $ done ()

input :: Coroutine r v d v
input = Coroutine ($ In $ \s -> done s)

produce :: [a] -> Coroutine r u a ()
produce = foldr coOut (done ())

consume :: Coroutine [t] u t a -> [t]
consume c = apply c con
 where
  con (Out x next) = x : consume next
  con (Done _    ) = []
  -- con (In ff) = consume $ coIn ff

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = coIn (\v -> if p v then coOut v $ filterC p else filterC p)

limit :: Int -> Coroutine r v v ()
limit n | n > 0 = coIn $ \v -> coOut v $ limit $ n - 1
        | otherwise = done ()

suppress :: Int -> Coroutine r v v ()
suppress 0 = coIn $ \v -> coOut v $ suppress 0 
suppress n = coIn $ \v -> suppress $ n - 1

add :: Coroutine r Int Int ()
add = coIn $ \i1 -> coIn $ \i2 -> coOut (i1+i2) add

add1 :: Coroutine r Int Int ()
add1 = coIn $ \i1 -> coOut (i1+1) add1

foldC :: Int ->  Coroutine r Int Int ()
foldC acc = coIn $ \v -> coOut (acc + v) $ foldC (acc + v)

duplicate :: Coroutine r v v ()
duplicate = coIn $ \v1 -> coOut v1 $ coOut v1 duplicate

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

zeros = coOut 0 zeros

p1 = filterC even >>> limit 5 
p2 = zeros >>> add1 >>> foldC 0 >>> foldC 0
p3 = duplicate >>> add
p4 = duplicate >>> suppress 1 >>> add

