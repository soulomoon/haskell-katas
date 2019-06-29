module SymbolicDifferentiationOfPrefixExpressions (diff) where

import           Text.Parsec
import qualified Text.Parsec.Token             as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import Data.Functor.Identity
import Data.Either
import Control.Monad
import Debug.Trace


langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { 
    Tok.commentStart = ""
  , Tok.commentEnd = ""
  , Tok.commentLine = ""
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = [] 
  , Tok.reservedOpNames = reservedNames
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens = Tok.parens lexer
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

variable :: Parser Term
variable = symbol "x" >> return X

number :: Parser Term
number = Imm <$> Tok.integer lexer


symbol :: String -> Parser String
symbol = Tok.symbol lexer

node :: Parser Term
node = do
  symbol "("
  op <- choice $ map symbol reservedNames
  r <- operator op
  symbol ")"
  return r

operator :: String -> Parser Term
operator s 
  | s `elem` binOp = 
    case s of 
      "+" -> binTerm Add 
      "-" -> binTerm Sub
      "*" -> binTerm Mul
      "/" -> binTerm Div
      "^" -> binTerm Pow
  | s `elem` uneOp =
    case s of
      "exp" -> uneTerm Exp
      "ln" -> uneTerm Ln
      "tan" -> uneTerm Tan
      "sin" -> uneTerm Sin
      "cos" -> uneTerm Cos
  | otherwise = error $ "unknow operator " ++ s

binTerm :: (Term -> Term -> Term) -> Parser Term
binTerm b = liftM2 b term term

uneTerm :: (Term -> Term) -> Parser Term
uneTerm b = b <$> term


binOp = [
  "+",
  "-",
  "*", 
  "/", 
  "^"]

uneOp = [
  "exp",
  "ln",

  "tan",
  "sin",
  "cos"
        ]

reservedNames = binOp ++ uneOp

exp :: String -> Parser Term
exp "sin" = liftM Sin term 
exp "cos" = liftM Cos term 
exp "tan" = liftM Tan term 
exp "ln" = liftM Ln term 
exp "exp" = liftM Exp term 
exp op = error $ "op " ++ op ++ "undefined" 

term :: Parser Term
term = node <|> variable <|> number

parseTerm :: String -> Either ParseError Term
parseTerm = parse term "error" 

ppt :: String -> IO ()
ppt s = do 
  print "------"
  let t = parseTerm s
  print t
  case t of
    Right v -> print $ reduce $ diffTerm v
    Left _ -> print "parse error"


data Term = Imm Integer
    | X 
    | Cos Term 
    | Sin Term  
    | Tan Term  
    | Exp Term
    | Ln Term

    | Pow Term Term 
    | Mul Term Term
    | Add Term Term
    | Sub Term Term
    | Div Term Term

wrap x = "(" ++ x ++ ")"

instance Show Term where
  show (Imm x) = show x
  show X = "x"
  show (Cos (Add y X)) = wrap $ "cos " ++ show (Add X y)
  show (Cos x) = wrap $ "cos " ++ show x
  show (Sin (Add y X)) = wrap $ "sin " ++ show (Add X y)
  show (Sin x) = wrap $ "sin " ++ show x
  show (Tan x) = wrap $ "tan " ++ show x
  show (Exp x) = wrap $ "exp " ++ show x
  show (Ln x) = wrap $ "ln " ++ show x

  show (Pow x y) = wrap $ "^ " ++ show x ++ " " ++ show y
  show (Mul x y) = wrap $ "* " ++ show x ++ " " ++ show y
  show (Add x y) = wrap $ "+ " ++ show x ++ " " ++ show y
  show (Sub x y) = wrap $ "- " ++ show x ++ " " ++ show y
  show (Div (Imm x) (Imm y)) = show $ (fromIntegral x :: Float) / (fromIntegral y :: Float)
  show (Div x y) = wrap $ "/ " ++ show x ++ " " ++ show y

diffTerm :: Term -> Term
diffTerm (Imm _) = Imm 0
diffTerm X = Imm 1
diffTerm (Cos x) = Mul (diffTerm x) $ Mul (Imm (-1)) $ Sin x
diffTerm (Sin x) = Mul (diffTerm x) (Cos x) 
diffTerm (Tan x) = Mul (diffTerm x) $ Add (Imm 1) (Pow (Tan x) (Imm 2))
diffTerm t@(Exp x) = Mul nx t
  where nx = reduce $ diffTerm x
diffTerm (Ln x) = Mul (diffTerm x) $ Div (Imm 1) x
diffTerm (Pow x y) = diffBin Pow x y
diffTerm (Mul x y) = diffBin Mul x y
diffTerm (Add x y) = diffBin Add x y
diffTerm (Sub x y) = diffBin Sub x y
diffTerm (Div x y) = diffBin Div x y


countLevel :: Term -> Integer
countLevel X = 1 
countLevel (Imm _) = 0
countLevel (Cos x) = 1 + countLevel x
countLevel (Sin x) = 1 + countLevel x
countLevel (Tan x) = 1 + countLevel x
countLevel (Exp x) = 1 + countLevel x
countLevel (Ln x) = 1 + countLevel x
countLevel (Pow x y) = 1 + maximum [countLevel x, countLevel y]
countLevel (Add x y) = 1 + maximum [countLevel x, countLevel y]
countLevel (Mul x y) = 1 + maximum [countLevel x, countLevel y]
countLevel (Sub x y) = 1 + maximum [countLevel x, countLevel y]
countLevel (Div x y) = 1 + maximum [countLevel x, countLevel y]

diffBin con x y = 
  case con x y of
    Pow _ _ -> Mul nx $ Mul y (Pow x $ Sub y $ Imm 1) 
    Mul _ _ -> Add (Mul nx y) $ Mul ny x 
    Add _ _ -> Add nx ny
    Sub _ _ -> Sub nx ny
    Div _ _ -> Div (Sub (Mul nx y) (Mul x ny)) (Pow y (Imm 2))
  where nx = diffTerm x
        ny = diffTerm y

exchange :: Term -> Term
exchange (Add x y) = if countLevel nx <= countLevel ny then Add nx ny else Add ny nx
  where nx = exchange x
        ny = exchange y
exchange (Mul x y) = if countLevel nx <= countLevel ny then Mul nx ny else Mul ny nx
  where nx = exchange x
        ny = exchange y
exchange (Pow x y) = Pow (exchange x) (exchange y)
exchange (Sub x y) = Sub (exchange x) (exchange y)
exchange (Div x y) = Div (exchange x) (exchange y)
exchange (Sin x) = Sin (exchange x)
exchange (Cos x) = Cos (exchange x)
exchange (Exp x) = Exp (exchange x)
exchange (Tan x) = Tan (exchange x)
exchange (Ln x) = Ln (exchange x)
exchange x = x

shrink :: Term -> Term
shrink (Add (Imm x) (Add (Imm y) z)) = Add (Imm (x + y)) $ shrink z
shrink (Add (Imm x) (Sub (Imm y) z)) = Sub (Imm (x + y)) $ shrink z
shrink (Mul (Imm x) (Mul (Imm y) z)) = Mul (Imm (x * y)) $ shrink z
shrink (Mul (Imm x) (Div (Imm y) z)) = Div (Imm (x * y)) $ shrink z
shrink t@(Mul x (Pow yx (Imm yy)))
  | yy < 0 = Div x $ Pow yx (Imm $ negate yy)
  | otherwise = t
shrink x = x

reduce = shrink . reduce' . shrink . exchange 

reduce' :: Term -> Term
reduce' t@(Mul x y) = 
  case (nx, ny) of 
    (Imm x, Imm y) -> Imm $ x * y
    (_, Imm 1) -> nx
    (Imm 1, _) -> ny
    (Imm 0, _) -> Imm 0
    (_, Imm 0) -> Imm 0
    _ -> Mul nx ny
  where nx = reduce x
        ny = reduce y
reduce' (Div x (Imm 1)) = reduce x
reduce' (Div x y) = Div (reduce x) (reduce y)
reduce' t@(Add x y) = 
  case (nx, ny) of
    (Imm 0, _) -> ny
    (_, Imm 0) -> nx
    (Imm x, Imm y) -> Imm $ x + y
    _ -> Add nx ny
  where nx = reduce x
        ny = reduce y
reduce' (Sub (Imm x) (Imm y)) = Imm $ x - y
reduce' (Sub x (Imm 0)) = reduce x
reduce' (Sub x y) = reduce (Add x (Mul y (Imm (-1))))
reduce' t@(Pow x y) = 
  case (nx, ny) of
    (Imm x, Imm y) -> Imm (x ^ y)
    (Imm 0, _) -> Imm 0
    (_, Imm 0) -> Imm 1
    (_, Imm 1) -> nx
    (Imm 1, _) -> Imm 1
    _ -> Pow nx ny
  where nx = reduce x
        ny = reduce y
reduce' (Exp x) = case reduce x of
                   Imm 0 -> Imm 1
                   _ -> Exp $ reduce x
reduce' x = x


diff :: String -> String
diff = show . reduce . diffTerm . fromRight X . parseTerm 

main :: IO ()
main = do
  -- ppt "5"
  -- ppt "x"
  -- ppt "(+ x x)"
  -- ppt "(- x x)"
  -- ppt "(* x 2)"
  ppt "(/ x 2)"
  -- ppt "(^ x 2)"
  -- ppt "(tan x)"
  -- ppt "(ln x)"
  -- ppt "(/ 2 (+ 1 x))"
  return ()



