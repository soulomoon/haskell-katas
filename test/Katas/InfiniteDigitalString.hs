module Katas.InfiniteDigitalString (spec) where

import qualified Kyu2.InfiniteDigitalString as User
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "InfiniteDigitalString" $ do
  describe "Example test" $ do
    it "should pass fixed tests" $ do
      User.findPosition "456" `shouldBe` 3
      User.findPosition "454" `shouldBe` 79
      User.findPosition "455" `shouldBe` 98
      User.findPosition "910" `shouldBe` 8
      User.findPosition "9100" `shouldBe` 188
      User.findPosition "99100" `shouldBe` 187
      User.findPosition "00101" `shouldBe` 190
      User.findPosition "001" `shouldBe` 190
      User.findPosition "00" `shouldBe` 190
      User.findPosition "123456789" `shouldBe` 0
      User.findPosition "1234567891" `shouldBe` 0
      User.findPosition "123456798" `shouldBe` 1000000071
      User.findPosition "10" `shouldBe` 9
      User.findPosition "53635" `shouldBe` 13034
      User.findPosition "040" `shouldBe` 1091
      User.findPosition "11" `shouldBe` 11
      User.findPosition "99" `shouldBe` 168
      User.findPosition "667" `shouldBe` 122
      User.findPosition "0404" `shouldBe` 15050
      User.findPosition "949225100" `shouldBe` 382689688
      User.findPosition "58257860625" `shouldBe` 24674951477
      User.findPosition "3999589058124" `shouldBe` 6957586376885
      User.findPosition "555899959741198" `shouldBe` 1686722738828503
      User.findPosition "01" `shouldBe` 10
      User.findPosition "091" `shouldBe` 170
      User.findPosition "0910" `shouldBe` 2927
      User.findPosition "0991" `shouldBe` 2617
      User.findPosition "09910" `shouldBe` 2617
      User.findPosition "09991" `shouldBe` 35286
      User.findPosition "24201734" `shouldBe` 2409926

      
