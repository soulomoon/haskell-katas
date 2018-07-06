module Kyu6.Sieve
    ( primes
    )
where
import           Data.List

primes :: Int -> [Int]
primes n
    | n == 0 = []
    | otherwise = foldl
        (flip (\x -> (\\ (takeWhile (<= n) $ map (x *) [2 ..]))))
        [2 .. n]
        [2 .. n]

