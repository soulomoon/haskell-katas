module Katas.AdditionCommutes where

import Kyu1.AdditionCommutes
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "AdditionCommutes" $ do
  describe "My own tests" $ do
    it "My first test" $ do
      "Write your own tests here!" `shouldNotBe` "Good luck!"
