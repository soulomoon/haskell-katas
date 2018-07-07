module Katas.SumByFactors where
import Kyu4.SumByFactors
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Sum by Factors" $ do
        it "1st series" $ do
            -- sumOfDivided([12, 15]) `shouldBe` [(2,12),(3,27),(5,15)]
            -- sumOfDivided([15,21,24,30,45]) `shouldBe` [(2,54),(3,135),(5,90),(7,21)]
            sumOfDivided([-29804,-4209,-28265,-72769,-31744]) `shouldBe` [(2,-61548),(3,-4209),(5,-28265),(23,-4209),(31,-31744),(53,-72769),(61,-4209),(1373,-72769),(5653,-28265),(7451,-29804)]
            