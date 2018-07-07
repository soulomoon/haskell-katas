module FunctionEvaluator where
    
evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = error "todo: evaluateFunction"