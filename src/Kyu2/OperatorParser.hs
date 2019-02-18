{-# LANGUAGE DeriveFunctor #-}

module OperatorParser
    ( OpTree(..)
    , Associativity(..)
    , op
    , foldTree
    , parseOperators
    , module Text.ParserCombinators.ReadP
    )
where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Debug.Trace
import Test.Hspec
import Data.List
import Data.Char

-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b = Op (OpTree a b) a (OpTree a b)
                | Term b
                deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
                     deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree f (Op l a r)= f a (foldTree f l) (foldTree f r)
foldTree f (Term b) = b

-- | Return a parser such that: given 'op s a', if s matches, the parser 
-- | returns a.
op :: String -> a -> ReadP a
op s a = do
    _ <- string s
    return a

-- | Accept two arguments: 
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity; 
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree. 
parseOperators :: (Show a, Show b) => [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators xss pb = parseOperators' xss <* skipSpaces where
  parseOperators' [] = Term <$> pb
  parseOperators' (x:xs) = parseTermOrLower >>= parseFunc
          where 
              (parseFunc, pa) = case x of
                     (L pas) -> (parseL, choice pas)
                     (R pas) -> (parseR, choice pas)
                     (NoAssociativity pas) -> (parseN, choice pas)
              strip = between skipSpaces skipSpaces
              brackets = between (char '(') (char ')') . strip
              paa x = Op x <$> strip pa
              parseN x = paa x <*> parseOperators' xs <++ return x
              parseR x = paa x <*> (parseTermOrLower >>= parseL) <|> return x
              parseL x = (paa x <*> parseTermOrLower >>= parseL) <|> return x
              parseTermOrLower = brackets (parseOperators' xss) <|> parseOperators' xs





arithOps :: [Associativity [ReadP String]]
arithOps =
    map (fmap (map (\s -> op s s) . words))
        [ R "&& ||", NoAssociativity "< > =", L "+ -", L "* /", R "^"]

arithParser :: String -> String
arithParser s =
    case readP_to_S (parseOperators arithOps (munch1 isDigit) <* eof) s of
        [] -> ""
        xs -> brackets $ fst (last xs)

brackets :: OpTree String String -> String
brackets = foldTree (\o x y -> '(' : x ++ o ++ y ++ ")")

parsesTo :: Eq a => (a, String) -> [(a, String)] -> Bool
parsesTo _ [] = False
parsesTo x xs = last xs == x

exampleTree :: OpTree String String
exampleTree =
    Op (Op (Term "1") "*" (Term "2"))
       "+"
       (Op (Term "3") "/" (Op (Term "5") "^" (Term "6")))

main :: IO ()
main = hspec $ do
    describe "The op function" $ do
        it "parses an operator correctly" $
            readP_to_S (op ":<" ()) ":<xx" `shouldSatisfy` parsesTo ((), "xx")
        it "fails on incorrect input" $
            readP_to_S (op ":<" ()) ":xx" `shouldBe` []
        it "does not consume trailing whitespace" $
            readP_to_S (op ":<" ()) ":<  " `shouldSatisfy` parsesTo ((), "  ")
        it "fails if there is preceding whitespace" $
            readP_to_S (op ":<" ()) "   :<" `shouldBe` []

    describe "The foldTree function" $
        it "works as expected with the 'brackets' function" $
            brackets exampleTree `shouldBe` "((1*2)+(3/(5^6)))"

    describe "The parseOperators function" $ do
        it "parses a single term" $
            arithParser "(12)" `shouldBe` "12"
        it "parses expressions with no whitespace" $
            arithParser "1+1" `shouldBe` "(1+1)"
        it "parses expressions with latter square" $
            arithParser "1+((2 +1)) " `shouldBe` "(1+(2+1))"
        it "parses expressions with upfirst square" $
            arithParser "(1+(2 +1))+1 " `shouldBe` "((1+(2+1))+1)"
        it "fails if there is preceding whitespace" $
            arithParser "  1 + 1" `shouldBe` ""
        it "fails on incomplete expressions" $ do
            arithParser "* 4" `shouldBe` ""
        it "parses a simple expression separated by whitespace" $
            arithParser "1 \n+ 1" `shouldBe` "(1+1)"
        it "parses with correct precedence if lower precedence on the left" $
            arithParser "1 + 3 * 2" `shouldBe` "(1+(3*2))"
        it "parses with correct precedence if lower precedence on the right" $
            arithParser "1 * 3 + 2" `shouldBe` "((1*3)+2)"
        it "Noassosiate appear twice" $
            arithParser "1 > 3 > 2" `shouldBe` ""
        it "parses with left associativity correctly" $
            arithParser "2 - 5 - 9 - 5 * 4" `shouldBe` "(((2-5)-9)-(5*4))"
        it "parses right associativity correctly" $
            arithParser "1 + 2 ^ 4 ^ 5" `shouldBe` "(1+(2^(4^5)))"
        it "parses no associativity correctly" $ do
            arithParser "1 + 2 < 3 * 4" `shouldBe` "((1+2)<(3*4))"
        it "parses brackets correctly" $ do
            arithParser "2 * ( 3+1 ) / (2-4)" `shouldBe` "((2*(3+1))/(2-4))"
        it "parses a complex expression correctly" $
            arithParser "1+7/2 > 2*3+4 && 2*3/7+1 < 5^(5-2 ) || 4*3 > 2"
            `shouldBe`
            "(((1+(7/2))>((2*3)+4))&&(((((2*3)/7)+1)<(5^(5-2)))||((4*3)>2)))"
