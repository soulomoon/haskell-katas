module Kyu3.IntegerToNestedStructure (encode,decode) where
import Data.List

encode :: Int -> Int
encode = undefined

decode :: Int -> Int
decode = undefined

-- toList [] = []   
-- toList [n] = 


primesTo n
    | n == 0 = []
    | otherwise = foldl
        (flip (\x -> (\\ (takeWhile (<= n) $ map (x *) [2 ..]))))
        [2 .. n]
        [2 .. n]