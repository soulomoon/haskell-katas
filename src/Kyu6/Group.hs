module Kyu6.Group where


import Data.Monoid

class Monoid m => Group m where
  invert :: m -> m

--
-- a <> invert a == mempty
-- invert a <> a == mempty
--
-- -- Group homomorphism rules
-- embedLeft x <> embedLeft y == embedLeft (x <> y)
-- invert (embedLeft x) == embedLeft (invert x)
-- embedRight x <> embedRight y == embedRight (x <> y)
-- invert (embedRight x) == embedRight (invert x)

-- Commutative diagram rules
-- fst (embedLeft x) == x
-- snd (embedRight x) == x

instance (Group a, Group b) => Group (a, b) where
  invert (a, b) = (invert a, invert b)

embedLeft :: (Group a, Group b) => a -> (a,b)
embedLeft a = (a, mempty)
  
embedRight :: (Group a, Group b) => b -> (a,b)
embedRight b = (mempty, b)
