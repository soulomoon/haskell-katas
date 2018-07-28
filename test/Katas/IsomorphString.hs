module Katas.IsomorphString where

import Test.Hspec
import Kyu6.IsomorphString (isomorph)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "IsomorphString" $ do
  describe "Isomorph" $ do
    it "Example tests" $ do
      isomorph "ESTATE" "DUELED" `shouldBe` True
      isomorph "XXX" "YYY" `shouldBe` True
      isomorph "SEE" "SAW" `shouldBe` False
      isomorph "XXY" "XYY" `shouldBe` False
      -- Should handle words with more than 10 characters
      isomorph "abcdefghijk" "abcdefghijba" `shouldBe` False
