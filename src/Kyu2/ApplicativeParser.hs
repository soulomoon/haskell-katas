module Kyu2.ApplicativeParser where

import Data.Char
import Debug.Trace
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ map go . unP p
  where go (s,a) = (s, f a)
    

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) = pmap . const

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P pp
  where pp (x:xs)
          | p x = [(xs, x)]
          | otherwise = []
        pp _ = []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \s -> [(s, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P pp
  where 
    pp [] = []
    pp s = concat [[(s2, f a) | (s2, a) <- unP px s1] | (s1, f) <- unP pf s]


(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P pp
  where 
    pp [] = []
    pp s = concat [[(s2, a) | (s2, _) <- unP pb s1] | (s1, a) <- unP pa s]

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P pp
  where 
    pp [] = []
    pp s = concat [unP pb s1 | (s1, _) <- unP pa s]

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP = foldr (\ x -> (<@>) ((:) <#> charP x)) (inject [])

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(<<>>) (P a) (P b) = P (\x -> a x ++ b x)


infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = ((:) <#> p <@> many p) <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> many p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd $ filter pred (unP p cs)
                       where pred ("", _) = True
                             pred _ = False

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: (Show a) => Parser a -> String -> Maybe a
runParserUnique p cs = case filter pred $ traceShowId (unP p cs) of 
                         [("", x)] -> Just x
                         _ -> Nothing 
                       where pred ("", _) = True
                             pred _ = False


-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr exp = case exp of  
                 ConstE a -> a
                 BinOpE AddBO e1 e2 -> evalExpr e1 + evalExpr e2
                 BinOpE MulBO e1 e2 -> evalExpr e1 * evalExpr e2
                 NegE e -> negate $ evalExpr e
                 ZeroE -> 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
parseConst :: Parser Expr
parseConst = pmap (ConstE . (read::String -> Int)) $ some $ predP isDigit

parseNeg :: Parser Expr
parseNeg = charP '-' @> (NegE <#> parseExpr0)

parseZero :: Parser Expr
parseZero = charP 'z' @> inject ZeroE

parseBinOp :: Parser BinOp
parseBinOp = charP '+' @> inject AddBO <<>> charP '*' @> inject MulBO

parseBinOpExpr :: Parser Expr
parseBinOpExpr = flip BinOpE <#> (charP '(' @> parseExpr0 <@ charP ' ') <@> parseBinOp <@> (charP ' ' @> parseExpr0 <@ charP ')')

parseExpr0 :: Parser Expr
parseExpr0 = parseConst <<>> parseBinOpExpr <<>> parseNeg <<>> parseZero

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique parseExpr0 

main :: IO ()
main = do
  print $ parseExpr "--1"
  return ()
