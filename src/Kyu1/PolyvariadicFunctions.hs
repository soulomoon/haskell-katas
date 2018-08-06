{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE FunctionalDependencies #-}

module Kyu1.PolyvariadicFunctions where


class AddNum a where
    polyAdd' :: Int -> a
instance AddNum Int where
    polyAdd' a = a 
instance (i ~ Int, AddNum r) => AddNum (i -> r) where
    polyAdd' a x = polyAdd' $ a + x

-- `polyAdd` sums its arguments, all `Int`s.
polyAdd :: (AddNum a) => a
polyAdd = polyAdd' 0


class PolyWords a where
    polyWords' :: String -> a
instance PolyWords String where
    polyWords' a = a
instance (i ~ String, PolyWords r) => PolyWords (i -> r) where
    polyWords' a x = polyWords' $ a ++ m ++ x where m = if a == "" then a else " "

-- `polyWords` turns its arguments into a spaced string.
polyWords :: (PolyWords a) => a
polyWords = polyWords' ""


class PolyList a b | b -> a where
    polyList' :: [a] -> b
instance PolyList a [a] where
    polyList' ax = ax
instance PolyList a b => PolyList a (a -> b) where
    polyList' ax x = polyList' $ ax ++ [x]

-- `polyList` turns its arguments into a list, polymorphically.
polyList :: (PolyList a b) => b
polyList = polyList' []
