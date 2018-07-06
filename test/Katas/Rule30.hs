module Katas.Rule30 (spec) where

import Kyu5.Rule30
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "returns [1,1,1] on [1] after a single step" $
    rule30 [1] 1 `shouldBe` [1, 1, 1]
  it "returns [1, 1, 0, 0, 1] after two steps" $
    rule30 [1] 2 `shouldBe` [1, 1, 0, 0, 1]
    
  it "returns the list if the number of iterations is negative" $ property $ \xs (Positive n) ->
    rule30 xs (negate n) `shouldBe` xs
    
  it "interprets arbitrary numbers (not equal to 1) as 0" $ property $ \xs (Positive n) ->
    -- this should also work for negative n, but
    -- "a negative number of iteration never changes the initial sequence"
    rule30 xs n `shouldBe` rule30 xs n
    
  it "returns a list with the correct length" $ property $ \(NonEmpty xs) n ->
    let l = 2 * (max 0 n) + length xs
    in rule30 xs n `shouldSatisfy` (l ==) . length
  