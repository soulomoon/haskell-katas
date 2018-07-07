module Kyu4.MiddlePermutation where
import Data.List

middlePermutation :: String -> String
middlePermutation myString  = a !! (div (length a) 2 -1) where a = sort $ permutations myString