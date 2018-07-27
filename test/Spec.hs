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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
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
    
    
