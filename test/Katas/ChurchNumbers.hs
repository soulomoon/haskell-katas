module Katas.ChurchNumbers (spec) where

import Prelude hiding (succ, pred)
import Kyu4.ChurchNumbers
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ChurchNumbers" $ do
  describe "when using Chruch numbers" $ do
    it "pred 0 = 0" $ do
      eval (pred zero) `shouldBe` 0

    it "pred 1 = 0" $ do
      eval (pred (succ zero)) `shouldBe` 0

    it "pred 2 = 1" $ do
      eval (pred (succ (succ zero))) `shouldBe` 1

    it "1 - 1 = 0" $ do
      eval (succ(zero) `subt` succ(zero)) `shouldBe` 0  
