module Katas.Isogram where
import Kyu7.Isogram
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "isIsogram" $ do
    it "testing 'Dermatoglyphics'" $ shouldBe (isIsogram "Dermatoglyphics") True
    it "testing 'moose'" $ shouldBe (isIsogram "moose") False
    it "testing 'aba'" $ shouldBe (isIsogram "aba") False
