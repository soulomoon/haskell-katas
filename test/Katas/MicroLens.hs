module Katas.MicroLens where

import Prelude hiding (sum)
import Kyu1.MicroLens
import Data.Monoid
import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Basic lensing" $ do
    it "should view _1" $ shouldBe (view _1 (1,2)) 1
    it "should view _2" $ shouldBe (view _2 (1,2)) 2
    it "should set _1"  $ shouldBe (set _1 0 (1,2)) (0,2)
    it "should set _2"  $ shouldBe (set _2 0 (1,2)) (1,0)
  describe "Basic traversals" $ do
    it "should traverse" $ 
      shouldBe (toListOf elements [1,2,3]) [1,2,3]
    it "should miss" $
      shouldBe (preview elements []) (Nothing :: Maybe Int)
  describe "Basic mapping" $ do
    it "should map" $
      shouldBe (toListOf (elements . to succ) [1,2,3]) [2,3,4 :: Int]
  describe "Basic prisming" $ do
    it "should get Left" $ do
      shouldBe (preview _Left (Left 3)) (Just 3 :: Maybe Int)
    it "should fail Right" $ do
      shouldBe (preview _Left (Right 3)) (Nothing :: Maybe Int)
  describe "Isoing basic lenses" $ do
    it "should view _2" $ shouldBe (view (_flip . _1) (1,2)) 2
    it "should view _1" $ shouldBe (view (_flip . _2) (1,2)) 1
    it "should set _2"  $ shouldBe (set (_flip . _1) 0 (1,2)) (1,0)
    it "should set _1"  $ shouldBe (set (_flip . _2) 0 (1,2)) (0,2)
  -- describe "Use of modules" $ do
    -- it "Cannot use Control.Lens" $ do
    --   hidden [ Module "Control.Lens"
    --          , Module "Control.Lens.Action"
    --          , Module "Control.Lens.At"
    --          , Module "Control.Lens.Combinators"
    --          , Module "Control.Lens.Cons"
    --          , Module "Control.Lens.Each"
    --          , Module "Control.Lens.Empty"
    --          , Module "Control.Lens.Equality"
    --          , Module "Control.Lens.Extras"
    --          , Module "Control.Lens.Fold"
    --          , Module "Control.Lens.Getter"
    --          , Module "Control.Lens.Indexed"
    --          , Module "Control.Lens.Internal"
    --          , Module "Control.Lens.Internal.Action"
    --          , Module "Control.Lens.Internal.Bazaar"
    --          , Module "Control.Lens.Internal.ByteString"
    --          , Module "Control.Lens.Internal.Context"
    --          , Module "Control.Lens.Internal.Deque"
    --          , Module "Control.Lens.Internal.Exception"
    --          , Module "Control.Lens.Internal.FieldTH"
    --          , Module "Control.Lens.Internal.Fold"
    --          , Module "Control.Lens.Internal.Getter"
    --          , Module "Control.Lens.Internal.Indexed"
    --          , Module "Control.Lens.Internal.Instances"
    --          , Module "Control.Lens.Internal.Iso"
    --          , Module "Control.Lens.Internal.Level"
    --          , Module "Control.Lens.Internal.Magma"
    --          , Module "Control.Lens.Internal.Prism"
    --          , Module "Control.Lens.Internal.PrismTH"
    --          , Module "Control.Lens.Internal.Reflection"
    --          , Module "Control.Lens.Internal.Review"
    --          , Module "Control.Lens.Internal.Setter"
    --          , Module "Control.Lens.Internal.TH"
    --          , Module "Control.Lens.Internal.Zoom"
    --          , Module "Control.Lens.Iso"
    --          , Module "Control.Lens.Lens"
    --          , Module "Control.Lens.Level"
    --          , Module "Control.Lens.Loupe"
    --          , Module "Control.Lens.Operators"
    --          , Module "Control.Lens.Plated"
    --          , Module "Control.Lens.Prism"
    --          , Module "Control.Lens.Reified"
    --          , Module "Control.Lens.Review"
    --          , Module "Control.Lens.Setter"
    --          , Module "Control.Lens.TH"
    --          , Module "Control.Lens.Traversal"
    --          , Module "Control.Lens.Tuple"
    --          , Module "Control.Lens.Type"
    --          , Module "Control.Lens.Wrapped"
    --          ]
