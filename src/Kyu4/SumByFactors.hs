module Kyu4.SumByFactors where
import Data.List
import Debug.Trace
import Data.Set (fromList, toList) 

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = sumOfDivided' xs $ toList . fromList $ primeFactorsOf xs
sumOfDivided' xs = map (\y -> (y, sum $ filter (\x -> mod x y == 0) xs))
primeFactorsOf = concatMap $ primeFactors . abs
primeFactors :: Integer -> [Integer]
primeFactors n = case factors of
        [] -> [n]
        _  -> factors ++ primeFactors (div n (head factors))
    where factors = take 1 $ filter (\x -> mod n x == 0) [2 .. round $ sqrt $ fromIntegral n]