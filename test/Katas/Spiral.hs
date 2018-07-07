module Katas.Spiral (spec) where
import Kyu3.Spiral
import Test.Hspec
import Test.QuickCheck
import Data.List


-- You may use this type to display your output.
newtype ShowSpiral = ShowSpiral [[Int]] deriving Eq

instance Show ShowSpiral where
  show (ShowSpiral spiral) = unlines $
    ["<samp>"] ++ 
    [[if x == 1 then '0' else '.' | x <- row] | row <- spiral] ++
    ["</samp>"]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "testing size 5" $ ShowSpiral (spiralize 5) `shouldBe` ShowSpiral
      [[1,1,1,1,1],
       [0,0,0,0,1],
       [1,1,1,0,1],
       [1,0,0,0,1],
       [1,1,1,1,1]]
    it "testing size 8" $ ShowSpiral (spiralize 8) `shouldBe` ShowSpiral
      [[1,1,1,1,1,1,1,1],
       [0,0,0,0,0,0,0,1],
       [1,1,1,1,1,1,0,1],
       [1,0,0,0,0,1,0,1],
       [1,0,1,0,0,1,0,1],
       [1,0,1,1,1,1,0,1],
       [1,0,0,0,0,0,0,1],
       [1,1,1,1,1,1,1,1]]
    it "testing size 13" $ ShowSpiral (spiralize 13) `shouldBe` ShowSpiral
       [[1,1,1,1,1,1,1,1,1,1,1,1,1],
        [0,0,0,0,0,0,0,0,0,0,0,0,1],
        [1,1,1,1,1,1,1,1,1,1,1,0,1],
        [1,0,0,0,0,0,0,0,0,0,1,0,1],
        [1,0,1,1,1,1,1,1,1,0,1,0,1],
        [1,0,1,0,0,0,0,0,1,0,1,0,1],
        [1,0,1,0,1,1,1,0,1,0,1,0,1],
        [1,0,1,0,1,0,0,0,1,0,1,0,1],
        [1,0,1,0,1,1,1,1,1,0,1,0,1],
        [1,0,1,0,0,0,0,0,0,0,1,0,1],
        [1,0,1,1,1,1,1,1,1,1,1,0,1],
        [1,0,0,0,0,0,0,0,0,0,0,0,1],
        [1,1,1,1,1,1,1,1,1,1,1,1,1]]