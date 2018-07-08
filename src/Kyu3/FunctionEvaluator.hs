module Kyu3.FunctionEvaluator where
import Debug.Trace
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

evaluateFunction :: (Ord a, Show a) => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = innerEval $ traceShow n n
        where innerEval = ((M.fromAscList $ zip params $ map inner params) M.!)
              params = allParams f n
              inner n = case (f n) of 
                (Left i) -> i
                (Right (args, func)) -> func $ map innerEval args 

allParams :: (Ord a, Show a) => (a -> Either b ([a], [b] -> b)) -> a -> [a]
allParams f n = S.toList $ S.insert n (allParams' S.empty n)
    where allParams' accu k
            | S.member k accu = accu
            | otherwise = case (f k) of 
                            (Left i) -> accu
                            (Right (args, func)) -> S.union (S.foldl (\ac -> allParams' ac) (S.insert k accu) ars) ars where ars = S.fromList args
