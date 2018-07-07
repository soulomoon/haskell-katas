import Test.Hspec
import qualified Katas.Rule30
import qualified Katas.Sieve
import qualified Katas.Spiral
import qualified Katas.Parentheses
import qualified Katas.SumByFactors

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Katas.Rule30" Katas.Rule30.spec
    describe "Katas.Sieve" Katas.Sieve.spec
    describe "Katas.Spiral" Katas.Spiral.spec
    describe "Katas.Parentheses" Katas.Parentheses.spec
    describe "Katas.SumByFactors" Katas.SumByFactors.spec