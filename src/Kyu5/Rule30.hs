module Kyu5.Rule30
    ( rule30
    )
where


rule30 :: [Int] -> Int -> [Int]
rule30 cells n | n > 0     = iterate mRow cells !! n
               | otherwise = cells

pureNumber :: Int -> Int
pureNumber 1 = 1
pureNumber _ = 0
rule :: [Int] -> Int
rule [0, 0, 1] = 1
rule [0, 1, 0] = 1
rule [0, 1, 1] = 1
rule [1, 0, 0] = 1
rule [_, _, _] = 0

matchRule = rule . map pureNumber
change :: [Int] -> [Int]
change []               = []
change (l : c : r : []) = matchRule [l, c, r] : []
change (l : c : r : xs) = matchRule [l, c, r] : (change $ c : r : xs)

mRow s = change $ [0, 0] ++ s ++ [0, 0]

someFunc :: IO ()
someFunc = print (rule30 [1] 5)
