module Kyu3.Spiral where
import           Data.List
import           Data.Function

spiralize :: Int -> [[Int]]
spiralize n = foldl replaceNNth (replicate n (replicate n 1)) (cartProd n)

cartProd :: Int -> [[Int]]
cartProd n =
    [ [x, y]
    | x <- [0 .. n - 1]
    , y <- [0 .. n - 1]
    , zebra n [x, y]
    ] ++ zeroPoint n

zeroPoint n | odd n = []
            | even n = [[c-1, c]] where c = div n 2

zebra :: Int -> [Int] -> Bool
zebra c [x, y] = odd . compensate . maximumBy (compare `on` disWith) $ [x, y]
 where 
    compensate = (+) (restore 0) . restore
    restore = flip div 2 . (-) 1 . dis
    dis = abs . (-) (c - 1) . (* 2)
    disWith = (*)r . dis where r | inOpenLine = -1 | otherwise = 1
    inOpenLine = y - x == 1 && 2 * y <= c

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x : xs) | n == 0    = newVal : xs
                             | otherwise = x : replaceNth (n - 1) newVal xs

replaceNNth :: [[Int]] -> [Int] -> [[Int]]
replaceNNth matrix [y, x] = replaceNth x (replaceNth y 0 (matrix !! x)) matrix
