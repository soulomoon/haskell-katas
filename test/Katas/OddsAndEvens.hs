{-# LANGUAGE GADTs #-}
module Katas.OddsAndEvens (spec) where

import Kyu2.OddsAndEvens as S
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "(*)PlusOne" $ do
    it "evenPlusOne works" $ do
      fromOdd (S.evenPlusOne two) `shouldBe` 3
      fromOdd (S.evenPlusOne six) `shouldBe` 7
      fromOdd (S.evenPlusOne eight) `shouldBe` 9
    it "oddPlusOne works" $ do
      fromEven (S.oddPlusOne one) `shouldBe` 2
      fromEven (S.oddPlusOne five) `shouldBe` 6
      fromEven (S.oddPlusOne nine) `shouldBe` 10
  describe "(*)Plus(*)" $ do
    it "evenPlusEven works" $ do
      fromEven (S.evenPlusEven zero four) `shouldBe` 4
    it "evenPlusOdd works" $ do
      fromOdd (S.evenPlusOdd four three) `shouldBe` 7
    it "oddPlusOdd works" $ do
      fromEven (S.oddPlusOdd nine one) `shouldBe` 10
    it "oddPlusEven works" $ do
      fromOdd (S.oddPlusEven one two) `shouldBe` 3
  describe "(*)Times(*)" $ do
    it "evenTimesEven works" $ do
      fromEven (S.evenTimesEven four six) `shouldBe` 24
    it "evenTimesOdd works" $ do
      fromEven (S.evenTimesOdd zero nine) `shouldBe` 0
    it "oddTimesOdd works" $ do
      fromOdd (S.oddTimesOdd nine one) `shouldBe` 9
    it "oddTimesEven works" $ do
      fromEven (S.oddTimesEven three four) `shouldBe` 12

-- Representations to Integers
fromEven :: Even n -> Int
fromEven ZeroEven = 0
fromEven (NextEven n) = 2 + fromEven n
fromOdd :: Odd n -> Int
fromOdd OneOdd = 1
fromOdd (NextOdd n) = 2 + fromOdd n

-- Numbers for help during tests
zero = ZeroEven
one = OneOdd
two = NextEven zero
three = NextOdd one
four = NextEven two
five = NextOdd three
six = NextEven four
seven = NextOdd five
eight = NextEven six
nine = NextOdd seven
ten = NextEven eight
