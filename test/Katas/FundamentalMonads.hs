module Katas.FundamentalMonads where

import Kyu4.FundamentalMonads
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "FundamentalMonads" $ do
    it "type check" $ 1 `shouldBe` 1
