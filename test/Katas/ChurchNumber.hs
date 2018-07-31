module Katas.ChurchNumber where

import Test.Hspec
import Kyu3.ChurchNumber

findChurch fn x y = numerify $ fn (churchify x) $ churchify y

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ChurchNumber" $ do
  describe "Testing Church" $ do
    it "Works for examples" $ do
      findChurch churchAdd 1 3 `shouldBe` 4
      findChurch churchAdd 1 0 `shouldBe` 1
      findChurch churchMul 1 3 `shouldBe` 3
      findChurch churchMul 0 3 `shouldBe` 0
      findChurch churchMul 2 3 `shouldBe` 6
      findChurch churchPow 1 3 `shouldBe` 1
      findChurch churchPow 2 3 `shouldBe` 8
