module Kyu3.AlphabeticAnagrams where 
import Data.List

lexiPos :: String -> Integer
lexiPos lst@(x:xs) = lexiPos xs + first * div (factorial $ length xs) repeatedNum
    where first = fromIntegral . head . elemIndices x . sort $ lst
          factorial = product . flip take [1 ..]
          repeatedNum = product (map (\x -> fromIntegral $ factorial (length $ filter (==x) lst)) $ nub lst)
lexiPos _ = 1
