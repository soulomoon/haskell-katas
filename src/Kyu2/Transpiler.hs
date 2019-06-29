module Kyu2.Transpiler where

import Data.List
import Data.Char
import           Text.Parsec
import qualified Text.Parsec.Token             as Tok
import Text.Parsec.String (Parser)
import Data.Functor.Identity
import Debug.Trace


langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { 
    Tok.commentStart = ""
  , Tok.commentEnd = ""
  , Tok.commentLine = ""
  , Tok.nestedComments  = True
  , Tok.identStart      = letter <|> char '_'
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ""
  , Tok.opLetter        = oneOf ""
  , Tok.reservedNames   = [] 
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens = Tok.parens lexer
reserved = Tok.reserved lexer

nameOrNumber = trimSpaces $ Tok.identifier lexer <|> show <$> Tok.integer lexer



trimSpaces :: Parser a -> Parser a
trimSpaces  = (<* spaces)

lambda :: Parser Lambda
lambda = do
  char '{'
  spaces
  paras <- option [] $ try (nameOrNumber `sepBy1` trimSpaces (char ',') <* trimSpaces (string "->"))
  stmts <- option [] (nameOrNumber `sepBy` spaces)
  spaces
  char '}'
  spaces
  return $ Lambda paras stmts

expr :: Parser Exp
expr = ExpL <$> lambda <|> 
  ExpN <$> nameOrNumber

function :: Parser Func
function = do
  func <- expr
  spaces
  fronts <- option [] (parens $ expr `sepBy` trimSpaces (char ','))
  -- if last one
  end <- option Nothing (Just . ExpL  <$> lambda)
  spaces
  eof
  case end of 
    Nothing -> return $ Func func fronts
    Just x -> return $ Func func (fronts ++ [x])


parseTerm :: String -> Either ParseError Func
parseTerm = parse function "error" . trim

trim :: String -> String
trim = dropWhile (`elem` ['\n', ' ', '\t'])

ppt :: String -> IO ()
ppt s = do 
  print "--------------------------------------------------------"
  print $ trim s
  print "------"
  print $ parseTerm s
  print "------"
  print $ transpile s
  print "--------------------------------------------------------"

main :: IO ()
main = do
  -- ppt "call()"
  -- ppt "call(1)"
  -- ppt "call(a)"
  ppt "invoke  (       a    ,   b   )"
  -- ppt "{a->a}(1)"
  -- ppt "{a->a}(cde,y,z)"
  -- ppt "{a->a}(cde,y,z){}"
  -- ppt "{a->a}(cde,y,z){x,y,d -> stuff}"
  -- ppt "call({})"
  -- ppt "f({a->})"
  -- ppt "f({a->a})"
  -- ppt "call(\n){}"
  ppt "invoke  (       a    ,   b   ) { } "
  -- ppt "f(x){a->}"
  -- ppt "{}()"
  -- ppt "{a->a}(233)"
  ppt "fun { a, b -> a b }"
  ppt "run{a}"
  ppt "call (  ) "
  ppt "f({_->})"
  return ()

type NameOrNum = String 
-- name or lambda | pameters
data Func = Func Exp [Exp]
  deriving (Show)
data Exp = ExpN NameOrNum | ExpL Lambda
  deriving (Show)
-- lambdaparam lambdastmt
data Lambda = Lambda [NameOrNum] [NameOrNum]
  deriving (Show)

toTarget :: Func -> String
toTarget (Func e xs) = fromExp e ++ "(" ++ intercalate "," (map fromExp xs) ++ ")"
  where 
    fromExp (ExpN x) = x
    fromExp (ExpL x) = fromLambda x
    fromLambda (Lambda args stmts) = 
      "(" ++
      intercalate "," args ++
      ")" ++
      "{" ++
      concatMap (++";") stmts ++
      "}"


transpile :: String -> Either String String
transpile s = case parseTerm  $ traceShowId s of
                Left e -> Left "Hugh?"
                Right e -> toTarget <$> Right e

