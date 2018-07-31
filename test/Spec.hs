import Test.Hspec

import qualified Katas.Rule30
import qualified Katas.Sieve
import qualified Katas.Spiral
import qualified Katas.Parentheses
import qualified Katas.SumByFactors
import qualified Katas.MiddlePermutation
import qualified Katas.FunctionEvaluator
import qualified Katas.FundamentalMonads
import qualified Katas.IntegerToNestedStructure
import qualified Katas.OddsAndEvens
import qualified Katas.Church
import qualified Katas.ChurchNumbers
import qualified Katas.IsomorphString
import qualified Katas.Isomorphism
import qualified Katas.ISO
import qualified Katas.Isogram
import qualified Katas.ALaCarte
import qualified Katas.PC
import qualified Katas.ChurchNumber

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
    describe "Katas.ChurchNumber" Katas.ChurchNumber.spec
    describe "Katas.PC" Katas.PC.spec
    describe "Katas.ALaCarte" Katas.ALaCarte.spec
    describe "Katas.Isogram" Katas.Isogram.spec
    describe "Katas.ISO" Katas.ISO.spec
    describe "Katas.Isomorphism" Katas.Isomorphism.spec
    describe "Katas.IsomorphString" Katas.IsomorphString.spec
    describe "Katas.ChurchNumbers" Katas.ChurchNumbers.spec
    describe "Katas.Church" Katas.Church.spec
    describe "Katas.OddsAndEvens" Katas.OddsAndEvens.spec
    describe "Katas.IntegerToNestedStructure" Katas.IntegerToNestedStructure.spec
    describe "Katas.FundamentalMonads" Katas.FundamentalMonads.spec
    describe "Katas.FunctionEvaluator" Katas.FunctionEvaluator.spec
    describe "Katas.MiddlePermutation" Katas.MiddlePermutation.spec
    describe "Katas.Rule30" Katas.Rule30.spec
    describe "Katas.Sieve" Katas.Sieve.spec
    describe "Katas.Spiral" Katas.Spiral.spec
    describe "Katas.Parentheses" Katas.Parentheses.spec
    describe "Katas.SumByFactors" Katas.SumByFactors.spec
    
    
