module Kyu7.Isogram where
import Data.Char
import Data.List

isIsogram :: String -> Bool
isIsogram "" = True
isIsogram s = all isLetter s && length (nub $ map toLower s) == length s
