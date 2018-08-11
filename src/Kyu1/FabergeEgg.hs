module Kyu1.FabergeEgg where

import Data.List
import GHC.Integer.GMP.Internals (powModInteger)

mo = 998244353
a <**> b = (a * b) `rem` mo
a <--> b = (a - b) `rem` mo
b <^%> e = powModInteger b e mo

height :: Integer -> Integer -> Integer
height n m = take n m `mod` mo - 1
 where
  take n m
    | n >= m = 2 <^%> m
    | 2 * n > m = 2 <^%> m <--> take (m - n - 1) m
    | otherwise = sum
    $ scanl' (\s x -> s <**> (m + 1 - x) <**> (x <^%> (mo - 2))) 1 [1 .. n]
