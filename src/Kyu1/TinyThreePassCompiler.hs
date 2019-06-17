module TinyThreePassCompiler where

import qualified Text.Parsec as P
import Control.Monad.Identity
import Data.List
import Debug.Trace

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

-- store variable names
type Parser a = P.Parsec [Token] [String] a

advance :: P.SourcePos -> t -> [Token] -> P.SourcePos
advance pos _ r@(_:_) = P.incSourceColumn pos 1
advance pos _ [] = pos

satisfy :: (Token -> Bool) -> Parser Token
satisfy f = P.tokenPrim show
                      advance
                      (\c -> if f c then Just c else Nothing)

isNumber :: Token -> Bool
isNumber (TInt _) = True
isNumber _ = False

isString :: Token -> Bool
isString (TStr _) = True
isString _ = False

number :: Parser AST
number = 
  do
    (TInt i) <- satisfy isNumber 
    return $ Imm i

para :: Parser String
para = do
  (TStr s) <- satisfy isString
  return s

variable :: Parser AST
variable = 
  do
    (TStr s) <- satisfy isString
    xs <- P.getState
    case elemIndex s xs of
      Just n -> return $ Arg n
      _ -> P.parserFail "variable name not found"

symbol op = satisfy (==TChar op)
parens = P.between (satisfy (==TChar '(')) (satisfy (==TChar ')')) 
expr    = term   `P.chainl1` addop
term    = factor `P.chainl1` mulop
factor  = parens expr P.<|> number P.<|> variable
mulop   =   do{ symbol '*'; return Mul   }
       P.<|> do{ symbol '/'; return Div }
addop   =   do{ symbol '+'; return Add }
       P.<|> do{ symbol '-'; return Sub }

function :: Parser AST
function = do
  variables <- bracket (P.many para)
  -- update state
  P.putState variables
  expr 
  

bracket :: Parser a -> Parser a 
bracket = P.between (symbol '[') (symbol ']')

pass1 :: String -> AST
pass1 s = case P.runParser function [] "" (tokenize $ traceShowId s) of
            Right x -> x
            Left e -> error $ show e 

isImm :: AST -> Bool
isImm (Imm x) = True
isImm _ = False

opImm :: (AST -> AST -> AST) -> AST -> AST -> AST
opImm op ix@(Imm x) iy@(Imm y) 
 | op ix iy == Add ix iy  = Imm $ x + y
 | op ix iy == Sub ix iy  = Imm $ x - y
 | op ix iy == Mul ix iy  = Imm $ x * y
 | op ix iy == Div ix iy  = Imm $ x `div` y
 | otherwise = error "opImm not any of the binOp"
opImm _ _ _ = error "opImm not pass imm"

pass2' :: (AST -> AST -> AST) -> AST -> AST -> AST 
pass2' con x y
  | isImm nx && isImm ny = opImm con nx ny
  | otherwise = con nx ny
  where 
    nx = pass2 x
    ny = pass2 y

pass2 :: AST -> AST
pass2 (Add x y) = pass2' Add x y
pass2 (Div x y) = pass2' Div x y
pass2 (Sub x y) = pass2' Sub x y
pass2 (Mul x y) = pass2' Mul x y
pass2 x = x


data Operation = IM Int | AR Int | SW | PU | PO | AD | SU | MU | DI 
  deriving (Show)

getBinOp :: AST -> Operation
getBinOp (Add _ _) = AD
getBinOp (Sub _ _) = SU
getBinOp (Mul _ _) = MU
getBinOp (Div _ _) = DI

pass3a' :: (AST -> AST -> AST) -> AST -> AST -> [Operation]
pass3a' con l r = pass3a l ++ [PU] ++ pass3a r ++ [SW, PO, getBinOp $ con l r]

pass3a :: AST -> [Operation]
pass3a (Imm x) = [IM x]
pass3a (Arg x) = [AR x]
pass3a (Add x y) = pass3a' Add x y
pass3a (Div x y) = pass3a' Div x y
pass3a (Sub x y) = pass3a' Sub x y
pass3a (Mul x y) = pass3a' Mul x y

pass3 :: AST -> [String]
pass3 = map show . pass3a 

ex1_prog = "[ xx yy ] ( xx + yy ) / 2"
ex1_pass1 = Div (Add (Arg 0) (Arg 1)) (Imm 2)

ex2_prog = "[ x ] x + 2 * 5"
ex2_pass1 = Add (Arg 0) (Mul (Imm 2) (Imm 5))
ex2_pass2 = Add (Arg 0) (Imm 10)

ex3_prog = "[ a b ] a*a + b*b"
ex3_pass1 = Add (Mul (Arg 0) (Arg 0)) (Mul (Arg 1) (Arg 1))

ex4_prog = "[ a b ] a*a + b*b"
ex4_pass1 = Add (Mul (Arg 0) (Arg 0)) (Mul (Arg 1) (Arg 1))

compile :: String -> [String]
compile = pass3 . pass2 . pass1 

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0,_,_) = r0

interpret :: String -> [Int] -> Int
interpret s = simulate (compile s)


main :: IO ()
main = do
  -- print $ tokenize ex1_prog
  -- print $ pass1 ex1_prog
  -- print $ pass1 ex2_prog
  -- print $ pass1 ex3_prog

  -- print $ pass2 $ pass1 ex1_prog
  -- print $ pass2 $ pass1 ex2_prog
  -- print $ pass2 $ pass1 ex3_prog

  print $ pass2 $ pass1 ex4_prog

  
  -- print $ tokenize ex1_prog
  -- print $ interpret ex1_prog [1,1]
  -- print $ interpret ex2_prog [1]
  -- print $ interpret ex3_prog [1, 1]
