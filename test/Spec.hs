import Test.Hspec
import qualified Katas.Rule30
import qualified Katas.Sieve

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Katas.Rule30" Katas.Rule30.spec
    describe "Katas.Sieve" Katas.Sieve.spec