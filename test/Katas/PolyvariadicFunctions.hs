module Katas.PolyvariadicFunctions where

import Test.Hspec

import Kyu1.PolyvariadicFunctions

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "polyAdd" $ do
    it "Works for the examples" $ do
      (polyAdd 1 3 5 7 9 :: Int) `shouldBe` 25
      (polyAdd 1 2 3 :: Int) `shouldBe` 6
    it "Works for zero arguments" $ do
      (polyAdd :: Int) `shouldBe` 0
    it "Works for more arguments" $ do
      (polyAdd 32 652 231 9905 31 :: Int) `shouldBe` 10851
      (polyAdd 25 36 49 64 81 100 121 144 169 :: Int) `shouldBe` 789

  describe "polyWords" $ do
    it "Works for the examples" $ do
      (polyWords "This" "is" "a" "sentence." :: String) `shouldBe` "This is a sentence."
      (polyWords "Hello," "World!" :: String) `shouldBe` "Hello, World!"
    it "Works for zero arguments" $ do
      (polyWords :: String) `shouldBe` ""
    it "Works for more arguments" $ do
      (polyWords "a" :: String) `shouldBe` "a"
      (polyWords "Q" " " "W" :: String) `shouldBe` "Q   W"
      (polyWords "Hulle" "is nie" "honde" "nie!" :: String) `shouldBe` "Hulle is nie honde nie!"
      (polyWords "a^2" "+" "b^2" "=" "c^2" :: String) `shouldBe` "a^2 + b^2 = c^2"

  describe "polyList" $ do
    it "Works for the examples" $ do
      (polyList 5 4 3 2 1 :: [Integer]) `shouldBe` [5,4,3,2,1]
      (polyList 'H' 'e' 'l' 'l' 'o' :: String) `shouldBe` "Hello"
    it "Works for zero arguments" $ do
      (polyList :: [[String]]) `shouldBe` []
      (polyList :: [Maybe (Int, Char)]) `shouldBe` []
    it "Works for more arguments" $ do
      (polyList 1 3 5 7 9 11 13 15 17 :: [Integer]) `shouldBe` [1,3,5,7,9,11,13,15,17]
      (polyList EQ GT LT LT GT EQ LT :: [Ordering]) `shouldBe` [EQ,GT,LT,LT,GT,EQ,LT]
      (polyList True :: [Bool]) `shouldBe` [True]
