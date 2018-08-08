{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Katas.Singletons where

import Prelude hiding (take, drop, take, head, tail, index, zipWith, replicate, map, (++))
import qualified Prelude as P (map, take, (++))

import Kyu2.Singletons
import Test.Hspec

v0 = VNil
v1 = VCons 3 v0
v2 = VCons 2 v1
v3 = VCons 1 v2
v4 = VCons 0 v3

n0 = SZero
n1 = SSucc n0
n2 = SSucc n1
n3 = SSucc n2
n4 = SSucc n3

toList :: Vec a n -> [a]
toList VNil = []
toList (VCons v l) = v : toList l

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "index" $ do
    it "Index finds the correct element" $ do
      index n0 v4 `shouldBe` 0
      index n2 v3 `shouldBe` 3
  describe "drop" $ do
    it "Drop 0 items" $ do
      index n0 (drop n0 v4) `shouldBe` 0
      index n1 (drop n0 v4) `shouldBe` 1
    it "Drop n items" $ do
      toList (drop n1 v4) `shouldBe` [1,2,3]
      toList (drop n4 v4) `shouldBe` []
      toList (drop n3 v4) `shouldBe` [3]
    it "Drop great than the list" $ do
      toList (drop n3 v1) `shouldBe` ([] :: [Int])
  describe "take" $ do
    it "take 0 items" $ do
      toList (take n0 v4) `shouldBe` ([] :: [Int])
    it "take n items" $ do
      toList (take n1 v4) `shouldBe` [0]
      toList (take n2 v4) `shouldBe` [0,1]
      toList (take n4 v4) `shouldBe` [0,1,2,3]
    it "take n+1 items" $ do
      toList (take n4 v2) `shouldBe` [2,3]
  describe "++" $ do
    it "Concat singleton lists" $ do
      toList (v1 ++ v1) `shouldBe` [3,3]
    it "Concat lists of different sizes" $ do
      toList (v1 ++ v2) `shouldBe` [3,2,3]
  describe "head" $ do
    it "head" $ do
      head v4 `shouldBe` 0
      head (drop n2 v4) `shouldBe` 2
  describe "tail" $ do
    it "tail" $ do
      toList (tail v3) `shouldBe` toList (drop n1 v3)
  describe "map" $ do
    it "map show" $ do
      toList (map show v4) `shouldBe` ["0","1","2","3"]
    it "map square" $ do
      toList (map (^2) v4) `shouldBe` [0,1,4,9]
  describe "replicate" $ do
    it "replicate 1" $ do
      toList (replicate () n0) `shouldBe` ([] :: [()])
    it "replicate 2" $ do
      toList (replicate 0 n2) `shouldBe` [0,0]
  describe "zipWith" $ do
    it "zipWith" $ do
      toList (zipWith (,) (replicate 0 n2) v2) `shouldBe` [(0,2),(0,3)]
