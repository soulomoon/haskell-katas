module Katas.FunctionEvaluator (spec) where
import Kyu3.FunctionEvaluator
import Test.Hspec

factorial i | i == 0    = Left 1
            | otherwise = Right ([i-1], (*i).head)

fibonacci i | i < 2     = Left i
            | otherwise = Right ([i-1, i-2], sum)
        
coinchange (a, i) | a == 0          = Left 1
                    | a < 0 || i == 0 = Left 0
                    | otherwise       = Right ([(a, i-1), (a-coinlist!!(i-1), i)], sum)
coinlist = [1, 3, 5, 10]

heigth (n, m) | m <= 0 || n <= 0 = Left 0
                | otherwise        = Right ([(n, m-1), (n-1, m-1)], (+1).sum)

foo  i | i <= 2    = Left 1
        | odd i     = Right ([6*i`div`7, 2*i`div`3], sum)
        | otherwise = Right ([i-1, i-3], sum)


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "FunctionEvaluator" $ do
    it "should work for some basic tests" $ do
        evaluateFunction factorial 5    `shouldBe` 120
        evaluateFunction factorial 20   `shouldBe` 2432902008176640000
        evaluateFunction fibonacci 10   `shouldBe` 55
        evaluateFunction coinchange (20,  length coinlist) `shouldBe` 28
        evaluateFunction heigth (2, 14) `shouldBe` 105
        evaluateFunction foo 20         `shouldBe` 253      
            
    it "should work for some advanced tests" $ do
        evaluateFunction fibonacci 100     `shouldBe` 354224848179261915075
        evaluateFunction coinchange (500, length coinlist) `shouldBe` 146948
        evaluateFunction heigth (100, 150) `shouldBe` 1427228946471605830963606579751332537625641011
        evaluateFunction foo (10^12-3)     `shouldBe` 10393063677856661930403634886614144354995461022573498
