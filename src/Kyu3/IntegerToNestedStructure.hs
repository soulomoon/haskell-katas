module Kyu3.IntegerToNestedStructure where
import Data.List
import qualified Data.Map as M


encode :: Int -> Int
encode n = sum $ zipWith (*) (map (2^) [0, 1..]) (tail $ dropWhile (==0) $ reverse $ encodeIter n)
    where 
        ps = M.fromAscList $ zip (primesTo n) [0 ..]
        encodeIter = concatMap mapToOrder . primeFactors
        mapToOrder 0 = [0]
        mapToOrder 1 = [1]
        mapToOrder 2 = [1, 0]
        mapToOrder m = [1] ++ encodeIter (ps M.! m) ++ [0]

decode :: Int -> Int
decode n = decodeIter ns [[]]
    where ns = reverse $ replicate ((count 1 xs) - (count 0 xs)+1) 0 ++ 1:(reverse $ toBinary n) where xs = toBinary n

decodeIter :: [Int] -> [[Int]] -> Int
decodeIter [] [[x]] = x
decodeIter (1:xs) ys = decodeIter xs $ []:ys
decodeIter (0:xs) ([]:y2:ys) = decodeIter xs $ (2:y2):ys
decodeIter (0:xs) ([x]:y2:ys) = decodeIter xs $ ((primes !! x):y2):ys
decodeIter xs (y:ys) 
    | length y > 1 = decodeIter xs $ [foldr (*) 1 y]:ys
    | otherwise = -1
decodeIter _ _ = -1

count :: Int -> [Int] -> Int
count x= length . elemIndices x

toBinary 0 = []
toBinary n | mod n 2 == 1 = toBinary (div n 2) ++ [1] | mod n 2 == 0 = toBinary (div n 2) ++ [0]

primeFactors :: Int -> [Int]
primeFactors n = case factors of
        [] -> [n]
        _  -> factors ++ primeFactors (div n (head factors))
    where factors = take 1 $ filter (\x -> mod n x == 0) [2 .. round $ sqrt $ fromIntegral n]

primesTo m = (takeWhile (<= m) primes)

primes =1: 2 : oddPrimes
    where 
    oddPrimes = sieve [3,5..] 9 oddPrimes
    sieve (x:xs) q ps@ ~(p:pt)
        | x < q = x : sieve xs q ps
        | otherwise = sieve [x | x <- xs, rem x p /= 0] (head pt^2) pt