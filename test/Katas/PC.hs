{-# LANGUAGE ScopedTypeVariables #-}

module Katas.PC where
import Kyu1.PC
import Test.Hspec
import Test.QuickCheck
import System.Timeout
import Data.Proxy

test :: (Nat n, Num n, Eq n, Show n) => Proxy n -> Spec
test (p :: Proxy n) = do
  it "0 + x = x" $
    property (\(NonNegative x) -> 0 + from x == from x)
  it "successor x + y = successor (x + y)" $
    property (\(NonNegative x) (NonNegative y) ->
      successor (from x) + from y == successor (from x + from y))
  it "0 * x = 0" $
    property (\(NonNegative x) -> 0 * from x == 0)
  it "0 - x = 0" $
    property (\(NonNegative x) -> 0 - from x == 0)
  it "x - 0 = x" $
    property (\(NonNegative x) -> from x - 0 == from x)
  it "successor x - successor y = x - y" $
    property (\(NonNegative x) (NonNegative y) ->
      successor (from x) - successor (from y) == from x - from y)
  it "successor x * y = y + (x * y)" $
    property (\(NonNegative x) (NonNegative y) -> 
      successor (from x) * from y == from y + from x * from y)
  it "x `pow` 0 == 1" $
    property (\(NonNegative x) -> from x `pow` 0 == 1)
  it "2 ^ 3 == 8" $
    2 `pow` 3 `shouldBe` from 8
  it "3 ^ 2 == 9" $
    3 `pow` 2 `shouldBe` from 9
  it "pred 0 == Nothing" $
    pred (from 0) `shouldBe` Nothing
  it "pred (successor x) == Just x" $
    property (\(NonNegative x) -> 
      pred (successor (from x)) == Just (from x))
  it "iter 0 (1 +) x == x" $
    property (\(NonNegative x) -> iter 0 (1 +) (from x) == x)
  where
    from x = fromInteger x :: n
    pred = nat Nothing Just

main :: IO ()
main = hspec spec

spec :: Spec
spec = it "should work for some examples" $ do
  hspec $ describe "Peano" $ test (Proxy :: Proxy Peano)
  hspec $ describe "[()]" $ test (Proxy :: Proxy [()])
  hspec $ describe "Church" $ test (Proxy :: Proxy Church)
  hspec $ describe "Scott" $ test (Proxy :: Proxy Scott)
