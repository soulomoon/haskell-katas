module Katas.Sieve (spec) where

import Kyu6.Sieve
import Test.Hspec
import Text.Printf

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Primes up to 2" $
        it (printf "should return %s given %s as input" (show expected2) (show input2))
        $ primes input2 `shouldBe` expected2
    describe "Primes up to 3" $
        it (printf "should return %s given %s as input" (show expected3) (show input3))
        $ primes input3 `shouldBe` expected3
    where
    input2 = 2
    expected2 = [2]
    input3 = 3
    expected3 = [2,3]
