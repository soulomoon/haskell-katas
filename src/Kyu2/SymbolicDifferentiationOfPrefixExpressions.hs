module Kyu2.SymbolicDifferentiationOfPrefixExpressions (diff) where

import           Text.Parsec
import qualified Text.Parsec.Token             as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import Data.Functor.Identity
import Control.Monad

diff :: String -> String
diff = undefined

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
    deriving Show

diffTerm :: Term -> Term
diffTerm (Imm _) = Imm 0
diffTerm X = Imm 1
diffTerm (Cos x) = Mul (diffTerm x) $ Mul (Imm (-1)) $ Sin x
diffTerm (Sin x) = Mul (diffTerm x) (Cos x) 
diffTerm (Tan x) = Mul (diffTerm x) $ Div (Imm 1) $ Pow (Cos (diffTerm x)) (Imm 2)
diffTerm t@(Exp x) = Mul nx t
  where nx = reduce $ diffTerm x
diffTerm (Ln x) = Mul (diffTerm x) $ Div (Imm 1) x
diffTerm (Pow x y) = diffBin Pow x y
diffTerm (Mul x y) = diffBin Mul x y
diffTerm (Add x y) = diffBin Add x y
diffTerm (Sub x y) = diffBin Sub x y
diffTerm (Div x y) = diffBin Div x y

diffBin con x y = 
  case con x y of
    Pow _ _ -> Mul ny (Pow nx $ Sub ny $ Imm 1) 
    Mul _ _ -> Add (Mul nx y) $ Mul ny x 
    Add _ _ -> Add nx ny
    Sub _ _ -> Sub nx ny
    Div _ _ -> reduce (Mul x (Pow y (Imm (-1))))
  where nx = diffTerm x
        ny = diffTerm y
reduce :: Term -> Term
reduce t@(Mul x y) = 
  case (nx, ny) of 
    (Imm 1, _) -> ny
    (_, Imm 1) -> nx
    (Imm 0, _) -> Imm 0
    (_, Imm 0) -> Imm 0
    _ -> Mul nx ny
  where nx = reduce x
        ny = reduce y
reduce (Div x y) = reduce (Mul x (Pow y (Imm (-1))))
reduce t@(Add x y) = 
  case (nx, ny) of
    (Imm 0, _) -> ny
    (_, Imm 0) -> nx
    _ -> Add nx ny
  where nx = reduce x
        ny = reduce y
reduce (Sub x y) = reduce (Add x (Mul y (Imm (-1))))
reduce t@(Pow x y) = 
  case (nx, ny) of
    (Imm 0, _) -> Imm 0
    (_, Imm 0) -> Imm 1
    (_, Imm 1) -> nx
    (Imm 1, _) -> Imm 1
    _ -> Pow nx ny
  where nx = reduce x
        ny = reduce y
reduce (Exp x) = case reduce x of
                   Imm 0 -> Imm 1
                   _ -> Exp $ reduce x
reduce x = x

main :: IO ()
main = do
  ppt "(+ 1 (- 1 (exp 1)))"
  ppt "(* 1 (- 1 (exp 1)))"
  ppt "(* x (- 1 (exp 1)))"
  ppt "(* x x)"
  return ()



