module Katas.Parentheses (spec) where
import Kyu5.Parentheses (validParentheses)
import Test.Hspec
    
main :: IO ()
main = hspec spec

spec :: Spec
spec = it "should work for some examples" $ do
        validParentheses "()" `shouldBe` True
        validParentheses ")(" `shouldBe` False
        validParentheses ")"  `shouldBe` False
        validParentheses "(())((()())())"  `shouldBe` True