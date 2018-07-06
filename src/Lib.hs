module Lib 
    ( someFunc 
    ) where


rule30 :: [Int] -> Int -> [Int]
rule30 cells n
    | n > 0 = iterate mRow cells !! n
    | otherwise = cells

matchRule = rule . map pureNumber
pureNumber :: Int -> Int
pureNumber 1 = 1
pureNumber 0 = 0
pureNumber _ = 0

rule :: [Int] -> Int
rule [0,0,1] = 1
rule [0,1,0] = 1
rule [0,1,1] = 1
rule [1,0,0] = 1
rule [_,_,_] = 0

change :: [Int] -> [Int]
change [] = []
change (l:c:r:xs) = matchRule [l, c, r] : (change $ c : r : xs)
change (l:c:[]) = matchRule [l, c, 0]:[]


changeRow::[Int] -> [Int]
changeRow (c:[]) = matchRule [0, c, 0]:[]
changeRow (c:r:xs) = matchRule [0, c, r] : change (c:r:xs)

mRow s = changeRow $ [0] ++ s ++ [0]

someFunc :: IO ()
someFunc =  print (rule30 [1] 5)

