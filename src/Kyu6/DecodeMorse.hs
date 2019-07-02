module DecodeMorse where


import Data.Map.Strict ((!))
import Data.List
import Debug.Trace

decodeMorse :: String -> String
decodeMorse = unwords . map decodeWord . splitWords


decodeWord :: String -> String
decodeWord = concatMap decodeChar . words

decodeChar :: String -> String
decodeChar s = case traceShowId s of
                 ".-" -> "A"
                 "-..." -> "B"
                 "-.-." -> "C"
                 "-.." -> "D"
                 "." -> "E"
                 "..-." -> "F"
                 "--." -> "G"
                 "...."-> "H"
                 ".." -> "I"
                 ".---" -> "J"
                 "-.-" -> "K"
                 ".-.." -> "L"
                 "--" -> "M"
                 "-." -> "N"
                 "---" -> "O"
                 ".--." -> "P"
                 "--.-" -> "Q"
                 ".-." -> "R"
                 "..." -> "S"
                 "-" -> "T"
                 "..-" -> "U"
                 "...-" -> "V"
                 ".--" -> "W"
                 "-..-" -> "X"
                 "-.--" -> "Y"
                 "--.." -> "Z"
                 "...---..." -> "SOS"
                 "-.-.--" -> "!"
                 "-.-.-." -> ";"
                 ".-.-.-" -> "."



splitWords :: String -> [String]
splitWords s = map concat $ splitAll "   " $ group s

splitAll :: Eq a => a -> [a] -> [[a]]
splitAll a [] = []
splitAll a l@(x:xs) 
  | x == a = splitAll a xs
  | otherwise = takeWhile (/=a) l: splitAll a (dropWhile (/=a) l)


main = do
  print $ decodeMorse ".... . -.--   .--- ..- -.. ."


