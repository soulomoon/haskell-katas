import Test.Hspec
import qualified Katas.Rule30
import qualified Katas.Sieve
import qualified Katas.Spiral

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Katas.Rule30" Katas.Rule30.spec
    describe "Katas.Sieve" Katas.Sieve.spec
    describe "Katas.Spiral" Katas.Spiral.spec