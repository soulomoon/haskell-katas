module SimpleInteractiveInterpreter where


import           Text.Parsec
import qualified Text.Parsec.Token             as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Data.Map as M
import Data.Functor.Identity
import Control.Monad
import Control.Monad.State
import qualified Data.Foldable as F

type FnName = String 
type FnParameter= Identifier
data FunctionCall = FunctionCall FnName [Expression]
  deriving (Show)
data Function = 
  Function FnName [FnParameter] Expression
  deriving (Show)

type Identifier = String
type Number = Double
data Expression = 
    ExpressionF Factor
    | ExpressionE Operator Expression Expression 
  deriving (Show)
data Operator = 
  Plus | Minus | Times | Over | Mod
  deriving (Show)

data Assignment  = Assignment Identifier Expression
  deriving (Show)

data Term = TermE Expression | TermF Function | TermA Assignment
  deriving (Show)


langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { 
    Tok.commentStart = ""
  , Tok.commentEnd = ""
  , Tok.commentLine = ""
  , Tok.nestedComments  = False
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_"
  , Tok.opStart         = oneOf "+-*/%"
  , Tok.opLetter        = oneOf "+-*/%"  
  , Tok.reservedNames   = ["fn", "=>"] 
  , Tok.reservedOpNames = ["+", "-", "*", "/", "%"]
  , Tok.caseSensitive   = True
  }


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef
parens = Tok.parens lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
identifier = Tok.identifier lexer
infixOp s f = Ex.Infix (reservedOp s >> return f) Ex.AssocLeft
table :: Ex.OperatorTable String () Identity Expression
table = [
    [
     infixOp "*" (ExpressionE Times)
    , infixOp "/" (ExpressionE Over)
    , infixOp "%" (ExpressionE Mod)

    ],
    [
      infixOp "+" (ExpressionE Plus)
    , infixOp "-" (ExpressionE Minus)
    ]
  ]

number :: Parser Number
number = toDouble <$> Tok.naturalOrFloat lexer 
 where toDouble :: Either Integer Double -> Double
       toDouble (Left n) = fromInteger n
       toDouble (Right n) = n



factor :: Parser Factor 
factor = FactorN <$> number
          <|> FactorE <$> parens expression
          <|> try (FactorA <$> assignment)
          <|> try (FactorF <$> functionCall)
          <|> FactorI <$> identifier

function :: Parser Function
function = do
  reserved "fn"
  spaces
  funName <- identifier
  spaces
  args <- many identifier
  reserved "=>"
  spaces
  Function funName args <$> expression

functionCall :: Parser FunctionCall
functionCall = do
  funName <- identifier
  FunctionCall funName <$> 
    many1 (parens expression <|> (ExpressionF . FactorN <$> number) <|> (ExpressionF . FactorI <$> identifier))

assignment :: Parser Assignment 
assignment = do 
  name <- identifier 
  spaces
  char '='
  spaces
  Assignment name <$> expression


expression :: Parser Expression
expression =  Ex.buildExpressionParser table (ExpressionF <$> factor) <|> ExpressionF <$> factor 

term :: Parser Term
term = do 
  r <- TermF <$> function <|> TermE <$> expression
  eof
  return r

parseTerm :: String -> Either ParseError Term
parseTerm = parse term "Error"

type Interpreter = Env
type Result = Maybe Double 
type IResult = Either String (Result, Interpreter) 
type Env = (M.Map Identifier Value)
data Value = ValueF Function | ValueN Number
  deriving (Show)
type SResult a = StateT Env (Either String) a

putVal :: Identifier -> Value -> SResult Result
putVal name val = do
  env <- get 
  case (M.lookup name env, val) of
    (Just (ValueN _), ValueF _) -> 
      lift $ Left "function name already declared by value"
    (Just (ValueF _), ValueN _) -> 
      lift $ Left "value name already declared by function"
    _ -> put (M.insert name val env) >> return Nothing

getMaybeVal :: Identifier -> SResult (Maybe Value)
getMaybeVal name = gets $ M.lookup name 

getVal :: Identifier -> SResult Value
getVal name = do 
  val <- gets $ M.lookup name 
  case val of
      Nothing -> lift $ Left $ "identifier not found " ++ name
      Just val -> return val

testVal :: Identifier -> SResult Bool
testVal name = do 
  val <- gets $ M.lookup name 
  case val of
      Nothing -> return False
      Just val -> return True

getFunc :: Identifier -> SResult Function
getFunc name = do 
  val <- getVal name 
  case val of
    (ValueF fun) -> return fun
    _ -> lift $ Left $ "function " ++ name ++ " not found"

mevalTerm :: Term -> SResult Result
mevalTerm (TermE exp) = mevalExp exp
mevalTerm (TermF funDef) = mevalFunDef funDef 

mevalFunDef :: Function -> SResult Result
mevalFunDef func@(Function name paras exp) = 
  if not (checkValidVariable paras exp) 
     then lift $ Left "functionName is already taken by variable"
     else do val <- getMaybeVal name
             case val of 
               Just (ValueN _) -> lift $ Left "functionName is already taken by variable"
               _ -> putVal name (ValueF func)

mevalExp :: Expression -> SResult Result
mevalExp (ExpressionF factor) = mevalFactor factor
mevalExp (ExpressionE o expl expr) = do
  let op = case o of
             Plus -> (+)
             Minus -> (-)
             Over -> (/)
             Times -> (*)
             Mod -> \x y -> fromInteger $ toInteger (round x) `mod` toInteger (round y)
  (liftM2 . liftM2) op (mevalExp expl) (mevalExp expr)

checkValidVariable :: [Identifier] -> Expression -> Bool
checkValidVariable names = iter 
    where 
      iter (ExpressionF (FactorI i)) = i `elem` names 
      iter (ExpressionF (FactorE exp)) = iter exp
      iter (ExpressionF (FactorA (Assignment i exp))) = iter exp
      iter (ExpressionE op exp1 exp2 ) = iter exp1 && iter exp2
      iter _ = True

mevalFunctionCall :: FunctionCall -> SResult Result
mevalFunctionCall (FunctionCall fnName fnArgs) = do
  args <- F.foldrM maccFun [] fnArgs
  Just <$> makefuncall fnName args

maccFun :: Expression -> [Number] -> SResult [Number]
maccFun (ExpressionF (FactorI name)) args = do 
  val <- getVal name
  case val of
    (ValueN num) -> return $ num:args
    (ValueF (Function _ paras _)) -> do 
      arg <- makefuncall name $ take len args
      return $ arg: drop len args
        where len = length paras
maccFun exp args = do
  maybeArg <- mevalExp exp
  case maybeArg of
    Just arg -> return $ arg:args
    Nothing -> lift $ Left "argument is not in env"

-- make function call with args
makefuncall :: FnName -> [Number] -> SResult Number
makefuncall name args = do
  env <- get
  (Function _ paras exp) <- getFunc name
  let lenP = length paras
  let lenA = length args
  if lenP == lenA 
     then do
       let funEnv =  M.union (M.fromList (zip paras (map ValueN args))) env
       put funEnv
       funResult <- mevalExp exp
       -- return the env
       put env
       case funResult of 
         (Just num) -> return num
         Nothing -> lift $ Left $ "variable argument " ++ name ++ " is not in env"
    else lift $ Left $ "arguments number does not meet parameters number " ++ show lenP 
    ++ ": " ++ show paras ++ " vs " ++ show lenA ++ ": " ++ show args

mevalFactor :: Factor -> SResult Result
mevalFactor (FactorN n) = return $ Just n
mevalFactor (FactorE exp) = mevalExp exp
mevalFactor (FactorA ass) = mevalass ass
mevalFactor (FactorI i) = do
  val <- getVal i
  case val of
    (ValueF (Function name _ _)) -> Just <$> makefuncall name []
    (ValueN n) -> return $ Just n
mevalFactor (FactorF c) = mevalFunctionCall c

mevalass :: Assignment -> SResult Result
mevalass (Assignment name exp) = do
  val <- mevalExp exp 
  case val of 
    Nothing -> 
      lift $ Left "expresion return nothing in assigment" 
    (Just v) -> putVal name (ValueN v) >> return (Just v)

data Factor = 
  FactorN Number 
  | FactorE Expression
  | FactorA Assignment
  | FactorI Identifier
  | FactorF FunctionCall
  deriving (Show)

input :: String -> Interpreter -> IResult
input "" i = Right (Nothing, i)
input s i  
  | all (==' ') s = input "" i
  | otherwise = case parseTerm s of
    Right term -> runStateT (mevalTerm term) i
    Left s -> Left $ show s
  

ppt :: String -> Env ->  IO Env
ppt s i = do 
  putStrLn "----print original: "
  putStrLn s
  putStrLn "----print term: "
  let term = parseTerm s
  print term
  putStrLn "----print eval: "
  let result = input s i
  case result of 
    (Right (r, _)) -> print r
    _ -> print result

  putStrLn "----print env: "
  print i
  putStrLn "------------------------------------------------------------------"
  case result of
    Left _ -> return i
    Right (_, i1) -> return i1

newtype IOI = IOI {getIO :: IO Interpreter}

newInterpreter :: Interpreter
newInterpreter = M.empty


main :: IO ()
main = do
  ppt "x = 7" M.empty
    >>= ppt "x + 6"
    >>= ppt "x + 6" 
    >>= ppt "y + 7" 
    >>= ppt "x = 13 + (y = 3)" 
    >>= ppt "x = y = 7" 
    >>= ppt "x = 13 + (y = 3)"
    >>= ppt "fn avg x y => (x + y) / 2"
    >>= ppt "fn add x y => (x + y)"
    >>= ppt "avg 1 2"
    >>= ppt "a = 6"
    >>= ppt "b = 7"
    >>= ppt "avg a b"
    >>= ppt "fn echo x => x"
    >>= ppt "add echo 4 echo 3"
  return ()
