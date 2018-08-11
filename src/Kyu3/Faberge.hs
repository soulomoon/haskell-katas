module Kyu3.Faberge where

heigth :: Integer -> Integer -> Integer 
heigth n m = accu n 1 0 where 
    accu 0 t h = h
    accu i t h = accu (i-1) nt (h+nt) where nt = div (t * (m - n + i)) (n + 1 - i)

