{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module Katas.FoldingThroughAFixedPoint where

import Kyu2.FoldingThroughAFixedPoint
import Test.Hspec

l0 :: [Int]
l0 = [1,2,3]

t0 :: Tree Int
t0 = Bin (Leaf 1) 2 (Leaf 3)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "FoldingThroughAFixedPoint" $ do
  describe "Lists work" $ do
    it "should map from greatest to least"
      (leastList (greatestLeast (listGreatest l0)) `shouldBe` l0)
    it "should map from least to greatest"
      (greatestList (leastGreatest (listLeast l0)) `shouldBe` l0)
  describe "Trees work" $ do
    it "should map from greatest to least"
      (leastTree (greatestLeast (treeGreatest t0)) `shouldBe` t0)
    it "should map from least to greatest"
      (greatestTree (leastGreatest (treeLeast t0)) `shouldBe` t0)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Tree  a   = Leaf  a | Bin  (Tree a) a (Tree a) deriving (Eq, Show)
data TreeF a x = LeafF a | BinF x        a x        deriving Functor

leastTree :: Least (TreeF a) -> Tree a
leastTree = fold $ \case
  LeafF   a   -> Leaf   a
  BinF  l a r -> Bin  l a r

treeLeast :: Tree a -> Least (TreeF a)
treeLeast t = Least $ \k -> k $ case t of
  Leaf   a   -> LeafF a
  Bin  l a r -> BinF (fold k (treeLeast l))
                     a
                     (fold k (treeLeast r))

treeGreatest :: Tree a -> Greatest (TreeF a)
treeGreatest = unfold $ \case
  Leaf  a   -> LeafF a
  Bin l a r -> BinF l a r

greatestTree :: Greatest (TreeF a) -> Tree a
greatestTree (Greatest u s) = case u s of
  LeafF  a   -> Leaf a
  BinF l a r -> Bin (greatestTree (unfold u l))
                    a
                    (greatestTree (unfold u r))
