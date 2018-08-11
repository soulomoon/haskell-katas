module Katas.FabergeEgg where

import Kyu1.FabergeEgg
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Faberge" $ do
    it "should work for some basic tests" $ do
      height 1  51 `shouldBe` 51
      height 2  1  `shouldBe` 1
      height 4  17 `shouldBe` 3213
      height 16 19 `shouldBe` 524096
      height 23 19 `shouldBe` 524287
--

    it "should work for some advanced tests" $ do
      height 13  550 `shouldBe` 621773656
      height 531 550 `shouldBe` 424414512
--

    it "should work for some serious tests :)" $ do
      height 10000 100000 `shouldBe` 132362171
      height 80000 100000 `shouldBe` 805097588
      height 3000 (2^200) `shouldBe` 141903106
      height 80000 40000  `shouldBe` 616494770
      height 40000 80000  `shouldBe` 303227698
--
