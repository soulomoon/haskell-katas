module Katas.Lisp where

import Control.Applicative
import Control.Monad

import Kyu2.Lisp
import Test.Hspec

a ->> b = lispEval a `shouldBe` Just b
a =>> b = lispPretty a `shouldBe` Just b
gg a = do
  lispEval a `shouldBe` Nothing
  lispPretty a `shouldBe` Nothing
--

keep a = a =>> a

main :: IO ()
main = hspec $ do
  describe "Should parse literals" $ do
    it "should parse a single expression" $ do
      "1" ->> I32 1
      "/" ->> Sym "/"
      "true" ->> Boo True
      "()" ->> Nul
    it "should ignore whitespaces" $ do
      "  1,  " ->> I32 1
      "  true \n ,  \r\t" ->> Boo True
--

  describe "Should eval expressions" $ do
    it "should evaluate simple math expressions" $ do
      "(+ 1 1)" ->> I32 2
      gg "("
    it "should evaluate complex math expressions" $
      "(+ (+ 1 1) (+ 1 1))" ->> I32 4
    it "should compare values" $ do
      "(> 2 1)" ->> Boo True
      "(>= 2 1)" ->> Boo True
    it "should do boolean operations" $
      "(! true)" ->> Boo False
    it "should do if else" $
      "(if (> 2 1) 2 1)" ->> I32 2
    it "should work with lists" $ do
      "(list 1 2 3)" ->> Lst [I32 1, I32 2, I32 3]
      "(.. 1 100)" ->> Lst (I32 <$> [1 .. 100])
      "(reverse (.. 1 100))" ->> Lst (I32 <$> reverse [1 .. 100])

  describe "Should give errors" $
    it "should give errors" $
      "(begin ())" ->> Err

  describe "Should pretty print" $ do
    it "should keep good codes" $
      keep "(+ 1 1)"
    it "should reformat bad codes" $
      "(+  1 1)" =>> "(+ 1 1)"
--

