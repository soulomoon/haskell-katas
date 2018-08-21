{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Katas.Coroutine where

import Test.Hspec
import Test.QuickCheck (property)
import Kyu1.Coroutine
import Control.Applicative

l1, inf :: Coroutine r u Int  ()
l1 = produce [0..10]
inf = produce [0..]

run :: Coroutine a u d a -> a
run c = apply c (\case
                  Done x -> x
                  _ -> error "Not x")
          
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Coroutine" $ do
  describe "Library functions" $ do
    it "output" $ do
      consume (output 5) `shouldBe` [5]
      consume (output ()) `shouldBe` [()]
    it "input" $ do
      run ((10 <$ l1) >>> input) `shouldBe` 0
      run ((10 <$ inf) >>> input) `shouldBe` 0
    it "filter" $
      consume (l1 >>> filterC even) `shouldBe` [0,2,4,6,8,10]
    it "limit" $ do
      consume (l1 >>> limit (-1)) `shouldBe` []
      consume (l1 >>> limit 0) `shouldBe` []
      consume (l1 >>> limit 11) `shouldBe` [0..10]
      consume (l1 >>> limit 5)  `shouldBe` [0,1,2,3,4]
    it "suppress" $ do
      consume (l1 >>> suppress 0) `shouldBe` [0..10]
      consume (l1 >>> suppress 11) `shouldBe` []
      consume (l1 >>> suppress 5)  `shouldBe` [5..10]
    it "add" $ do
      consume (produce [1,2] >>> add) `shouldBe` [3]
      consume (produce [1,2,3,4] >>> add) `shouldBe` [3,7]
    it "duplicate" $ do
      consume (produce [1] >>> duplicate) `shouldBe` [1,1]
      consume (produce [1,2] >>> duplicate) `shouldBe` [1,1,2,2]
  describe "Programs" $ do
    it "Program 1" $ do
      consume (produce [1,2,3,4,6,8,10,12] >>> p1) `shouldBe` [2,4,6,8,10]
    it "Program 2" $ do
      consume (p2 >>> limit 5) `shouldBe` [1,3,6,10,15]
    it "Program 3" $ do
      consume (produce [1,2,3] >>> p3) `shouldBe` [2,4,6]
    it "Program 4" $ do
      consume (produce [0, 1, 2, 3] >>> p4) `shouldBe` [1,3,5]
