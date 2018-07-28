module Katas.Church (spec) where

import Prelude hiding (succ)
import Kyu4.Church
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Church" $ do
  describe "when using Chruch numbers" $ do
    it "1 + 0 equals 1" $ do
      eval (one + zero) `shouldBe` 1
    it "1 + 3 equals 4" $ do
      eval (one + three) `shouldBe` 4
    it "2 * 2 equals 4" $ do
      eval (succ one * succ one) `shouldBe` 4
    it "2 `pow` 3 equals 8" $ do
      eval (succ one `pow` succ (succ one)) `shouldBe` 8
