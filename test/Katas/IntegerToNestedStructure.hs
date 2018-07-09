module Katas.IntegerToNestedStructure where

import           Kyu3.IntegerToNestedStructure            ( encode
                                                          , decode
                                                          )
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Example tests" $ do
    it "encode" $ do
        encode 46 `shouldBe` 185
        encode 3 `shouldBe` 1
        encode 4 `shouldBe` 2
        encode 5 `shouldBe` 3
        encode 6 `shouldBe` 5
        encode 7 `shouldBe` 6
        encode 8 `shouldBe` 10
        encode 9 `shouldBe` 25
        encode 10 `shouldBe` 11
        encode 10000 `shouldBe` 179189987
        encode 10001 `shouldBe` 944359
        encode 10002 `shouldBe` 183722
        encode 10003 `shouldBe` 216499
        encode 10004 `shouldBe` 2863321
        encode 10005 `shouldBe` 27030299
        encode 10006 `shouldBe` 93754
        encode 10007 `shouldBe` 223005
        encode 10008 `shouldBe` 1402478
        encode 999983 `shouldBe` 1825387733
    it "decode" $ do
        decode 185 `shouldBe` 46
        decode 1 `shouldBe` 3
        decode 2 `shouldBe` 4
        decode 3 `shouldBe` 5
        decode 5 `shouldBe` 6
        decode 6 `shouldBe` 7
        decode 10 `shouldBe` 8
        decode 25 `shouldBe` 9
        decode 11 `shouldBe` 10
        decode 179189987 `shouldBe` 10000
        decode 944359 `shouldBe` 10001
        decode 183722 `shouldBe` 10002
        decode 216499 `shouldBe` 10003
        decode 2863321 `shouldBe` 10004
        decode 27030299 `shouldBe` 10005
        decode 93754 `shouldBe` 10006
        decode 223005 `shouldBe` 10007
        decode 1402478 `shouldBe` 10008
        decode 1825387733 `shouldBe` 999983
