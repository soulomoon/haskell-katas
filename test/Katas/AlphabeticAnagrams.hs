module Katas.AlphabeticAnagrams where

import Kyu3.AlphabeticAnagrams
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  describe "Basic lensing" $ do
    it "testing 'A'" $ shouldBe (lexiPos "A") 1    
    it "testing 'ABAB'" $ shouldBe (lexiPos "ABAB") 2   
    it "testing 'AAAB'" $ shouldBe (lexiPos "AAAB") 1   
    it "testing 'BAAA'" $ shouldBe (lexiPos "BAAA") 4
    it "testing 'QUESTION'" $ shouldBe (lexiPos "QUESTION") 24572
    it "testing 'BOOKKEEPER'" $ shouldBe (lexiPos "BOOKKEEPER") 10743
    it "testing 'IMMUNOELECTROPHORETICALLY'" $ shouldBe (lexiPos "IMMUNOELECTROPHORETICALLY") 718393983731145698173
