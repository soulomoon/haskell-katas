module Katas.Postfix where

import Test.Hspec
import Test.QuickCheck
import Kyu2.Postfix



main = hspec $ do
  describe "Programs" $ do
    it "Simple Programs" $ do
      (begin push 5 end) `shouldBe` 5
      (begin push 2 push 3 end) `shouldBe` 3
      (begin push 5 push 6 add end) `shouldBe` 11
      (begin push 1 push 1 add push 1 end) `shouldBe` 1
      (begin push 1 push 1 add push 2 add end) `shouldBe` 4 


    
    
