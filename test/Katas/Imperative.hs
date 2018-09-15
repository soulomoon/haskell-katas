module Katas.Imperative where

import Kyu2.Imperative (def, var, lit, while, (+=), (-=), (*=))
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "factorial" $ do
    it "should return the same as the functional one" $ do
      property $ \x -> factorial x `shouldBe` foldr (*) 1 [1..x]
  describe "howManyBetween" $ do
    it "should return the same as the functional one" $ do
      property $ \from to ->
        howManyBetween from to `shouldBe` (max 0 $ to - from - 1 :: Integer)

-- foldr (*) 1
factorial :: Integer -> Integer
factorial n = def $ do
  result <- var 1
  i      <- var n
  while i (>0) $ do
    result *= i
    i      -= lit 1
  return result
 
-- ((max 0 . subtract 1) .) . subtract
howManyBetween :: Integer -> Integer -> Integer
howManyBetween c n = def $ do
  result <- var 0
  i      <- var (c + 1)
  while i (<n) $ do
    result += lit 1
    i      += lit 1
  return result
