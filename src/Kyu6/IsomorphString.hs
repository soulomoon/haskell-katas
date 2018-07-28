module Kyu6.IsomorphString (isomorph) where
import Data.List

isomorph :: String -> String -> Bool
isomorph ax bx= and $ zipWith (\ a b -> elemIndices a ax == elemIndices b bx) ax bx 
