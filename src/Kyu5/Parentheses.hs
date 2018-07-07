module Kyu5.Parentheses (validParentheses) where

validParentheses :: String -> Bool
validParentheses = validParentheses' []
validParentheses' [] [] = True
validParentheses' [] (')':xs) = False
validParentheses' (1:ys) (')':xs) = validParentheses' ys xs
validParentheses' ys ('(':xs) = validParentheses' (1:ys) xs
validParentheses' _ _ = False
