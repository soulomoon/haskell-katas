module Katas.FundamentalMonads where

import Kyu4.FundamentalMonads
import Prelude hiding (Monad(..), Identity, Maybe(..), State, Reader, Writer)
import Test.Hspec

liftF :: Monad m => (a -> b) -> (a -> m b)
liftF f v = return (f v)

plusOne :: State Int Int
plusOne = State (\s -> (s, s+1))

timesTwo :: State Int Int
timesTwo = State (\s -> (s, s*2))

stateRID :: State a Int
stateRID = return 5 >>= liftF (+1)

stateLID :: State Int Int
stateLID = plusOne >>= return

changeArg :: Reader Int String
changeArg = Reader show

readerRID :: Reader a Int
readerRID = return 5 >>= liftF (+1)

readerLID :: Reader Int String
readerLID = changeArg >>= return

scribe :: String -> (Writer String String)
scribe s = Writer (reverse s, s)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(f >=> g) a = f a >>= g

writerLID  = return "test" >>= scribe
writerLIDR = scribe "test"
writerRIDR = scribe "test"
writerRID = scribe "test" >>= return


mASSOCL m = (m >=> m) >=> m 
mASSOCR m = m >=> (m >=> m)

rAssocl = mASSOCL scribe "test"
rAssocr = mASSOCL scribe "test"

tASSOCL m = mASSOCL m 0
tASSOCR m = mASSOCR m 0

iden = Identity
maybeassoc x = if x == 5 then Nothing else Just x
stateassoc x = State (\s -> (x, x + s))
rassoc x = Reader (\s -> if x == 0 then 10 else s - 1)




main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "FundamentalMonads" $ do
    it "Identity" $ do
      (return 5 >>= liftF (+1)) `shouldBe` (return 6 :: Identity Int)
      (Identity 5 >>= return) `shouldBe` Identity 5
      tASSOCL iden `shouldBe` tASSOCR iden
    it "Maybe" $ do
      (Just 5 >>= return) `shouldBe` Just 5
      (Nothing >>= return) `shouldBe` (Nothing :: Maybe Int)
      (Just 5 >>= liftF (+1)) >>= liftF (*2) `shouldBe` Just 5 >>= (\x -> liftF (+1) x >>= liftF (*2))
      tASSOCL maybeassoc `shouldBe` tASSOCR maybeassoc
    it "State" $ do
      runState stateRID () `shouldBe` runState (return 6) ()
      runState stateLID 0  `shouldBe` runState plusOne 0
      runState (tASSOCL stateassoc) 0 `shouldBe` flip runState 0 (tASSOCR stateassoc)
    it "Reader" $ do
      runReader readerRID () `shouldBe` runReader (liftF (+1) 5) ()
      runReader readerLID 10 `shouldBe` runReader changeArg 10
      (flip runReader 0 $ tASSOCL rassoc) `shouldBe` flip runReader 0 (tASSOCR rassoc)
    it "Writer" $ do
      (runWriter writerLID) `shouldBe` runWriter writerLIDR
      (runWriter writerRID) `shouldBe` runWriter writerRIDR
      (runWriter $ rAssocl) `shouldBe` runWriter rAssocr