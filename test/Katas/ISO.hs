module Katas.ISO where
import Kyu2.ISO

import Test.Hspec
import Test.QuickCheck

lrl :: ISO a b -> (a -> a)
lrl (ab, ba) = ba . ab

rlr :: ISO a b -> (b -> b)
rlr (ab, ba) = ab . ba

str :: String
str = "JoJo"

pa :: ISO
  (Either (Either Int Bool) String)
  (Either Int (Either Bool String))
pa = plusAssoc

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Example tests" $ do
  describe "isoLaw" $ do
    it "assoc" $ do
      lrl pa (Left  (Left     0)) `shouldBe` Left  (Left     0)
      lrl pa (Left  (Right True)) `shouldBe` Left  (Right True)
      lrl pa (Right          str) `shouldBe` Right          str
      rlr pa (Left             0) `shouldBe` Left             0
      rlr pa (Right (Left  True)) `shouldBe` Right (Left  True)
      rlr pa (Right (Right  str)) `shouldBe` Right (Right  str)
