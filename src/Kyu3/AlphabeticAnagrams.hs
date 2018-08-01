module Kyu3.AlphabeticAnagrams where 
import Data.List

factorial :: Int -> Integer
factorial = product . flip take [1 ..]

lexiPos :: String -> Integer
lexiPos lst@(x:xs) = lexiPos xs + first * div (factorial $ length xs) (repeatedNum lst)
    where first = fromIntegral $ head $ elemIndices x $ sort lst
lexiPos _ = 1

repeatedNum :: String -> Integer
repeatedNum lst = product (map (\x -> fromIntegral $ factorial (length $ filter (==x) lst)) $ nub lst)
