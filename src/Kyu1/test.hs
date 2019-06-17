module SimpleInteractiveInterpreter where


    import           Text.Parsec
    import qualified Text.Parsec.Token             as Tok
    import qualified Text.Parsec.Expr as Ex
    import Text.Parsec.String (Parser)
    import qualified Data.Map as M
    import Data.Functor.Identity
    import Control.Monad
    
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
    
    
    expr0 = "1.1%1.2"
    -- expr1 = "if false then 0 else succ (pred (succ 0))"
    -- expr2 = "if true then succ 0 else -1"
    
    parseTerm :: String -> Either ParseError Term
    parseTerm = parse term "Error"
    
    
    data Abstraction = Abstraction Function Interpreter
    type Interpreter = (M.Map Identifier Abstraction, M.Map Identifier Double)
    type Result = Maybe Double 
    type IResult = Either String (Result, Interpreter) 
    -- funDefs = fst
    -- vars = snd
    --
    instance Show Abstraction where
      show (Abstraction f _) = show f
    
    
    evalTerm :: Interpreter -> Term -> IResult
    evalTerm i (TermE exp) = evalExp i exp
    evalTerm i (TermF funDef) = evalFunDef i funDef
    
    
    data Factor = 
      FactorN Number 
      | FactorE Expression
      | FactorA Assignment
      | FactorI Identifier
      | FactorF FunctionCall
      deriving (Show)
    
    evalFactor :: Interpreter -> Factor -> IResult
    evalFactor i (FactorN n) = Right (Just n, i)
    evalFactor i (FactorE e) = evalExp i e
    evalFactor i (FactorA a) = evalAssignment i a
    evalFactor i@(funs, vars) (FactorI x) 
      | M.member x vars = Right (M.lookup x vars, i)
      | M.member x funs = evalFunctionCall i (FunctionCall x [])
      | otherwise =  Left "identifier is not a variable or function"
    evalFactor i (FactorF functionCall) = evalFunctionCall i functionCall
    
    evalFunctionCall :: Interpreter -> FunctionCall -> IResult
    evalFunctionCall i@(funs, _) (FunctionCall fnName fnArgs) = 
      if M.member fnName funs 
         then do
           args <- argResults
           let (Abstraction (Function _ parameters exp) (_, vars1)) = funs M.! fnName
           let nVars =  M.union (M.fromList (zip parameters args)) vars1
           if length parameters == length args
           then evalExp (funs, nVars) exp
           else Left $ "function calls args number wrong: " ++ show parameters ++ " vs " ++ show nVars
         else Left ("function name not exist in functioncall " ++ fnName ++ ":" ++ show funs)
      where 
        -- capture function calls
        argResults = foldr accFun (Right []) fnArgs
        accFun :: Expression -> Either String [Number] -> Either String [Number]
        accFun (ExpressionF (FactorI name)) accArgs = 
          if M.member name funs
             then do 
               args <- accArgs
               let (Abstraction (Function _ paras _) (_, vars)) = funs M.! name
               (r, _) <- evalFunctionCall (funs, vars) (FunctionCall name (map (ExpressionF . FactorN) $ take (length paras) args))
               case r of
                 Nothing -> Left "Nothing in argument"
                 Just x -> Right $ x:drop (length paras) args
             else do
               args <- accArgs
               (r, _) <- evalFactor i (FactorI name)
               case r of
                 Nothing -> Left "Nothing in argument"
                 Just x -> Right (x:args)
        accFun exp accArgs = do
          args <- accArgs
          (r, _) <- evalExp i exp
          case r of
            Nothing -> Left "Nothing in argument"
            Just x -> Right (x:args)
    
    
    
    
    
    evalAssignment :: Interpreter -> Assignment -> IResult
    evalAssignment i (Assignment name exp) = 
      if M.member name (fst i) 
         then Left "variable name is already taken by function" 
         else (do 
           (e, (funs, vars)) <- evalExp i exp 
           case e of
             Nothing -> Left "expression returned nothing in assignment"
             (Just x) -> return (e, (funs, M.insert name x vars)))
    
    evalExp :: Interpreter -> Expression -> IResult
    evalExp i (ExpressionF factor) = evalFactor i factor
    evalExp i (ExpressionE o expl expr) = do
      (l, i1) <- evalExp i expl
      (r, i2) <- evalExp i1 expr
      let op = case o of
                 Plus -> (+)
                 Minus -> (-)
                 Over -> (/)
                 Times -> (*)
                 Mod -> \x y -> fromInteger $ toInteger (round x) `mod` toInteger (round y)
      return (liftM2 op l r, i2)
    
    
    checkValidVariable :: Interpreter -> [Identifier] -> Expression -> Bool
    checkValidVariable env@(funs, vars) names = iter 
        where 
          iter (ExpressionF (FactorI i)) = elem i names || M.member i funs
          iter (ExpressionF (FactorE exp)) = iter exp
          iter (ExpressionF (FactorA (Assignment i exp))) = iter exp
          iter (ExpressionE op exp1 exp2 ) = iter exp1 && iter exp2
          iter _ = True
    
    
    evalFunDef :: Interpreter -> Function -> IResult
    evalFunDef i@(funs, vars) funDef@(Function name parameters exp)
      | not (checkValidVariable i parameters exp) = Left "variable not valid in function decalaration"
      | M.member name vars = Left "function named is already taken by a variable"
      | otherwise = let ni = (M.insert name (Abstraction funDef i) funs, vars)
                    in Right (Nothing, ni)
    
      
    
    input :: String -> Interpreter -> IResult
    input "" i = Right (Nothing, i)
    input s i  
      | all (==' ') s = input "" i
      | otherwise = case parseTerm s of
        Right term -> evalTerm i term
        Left s -> Left $ show s
    
    ppt :: String -> Interpreter ->  IO Interpreter
    ppt s i = do 
      putStrLn "----print original: "
      putStrLn s
      putStrLn "----print term: "
      let term = parseTerm s
      print term
      putStrLn "----print eval: "
      let result = input s i
      print result
    
      putStrLn "----print env: "
      print i
      putStrLn "------------------------------------------------------------------"
      case result of
        Left _ -> return i
        Right (_, i1) -> return i1
    
    newtype IOI = IOI {getIO :: IO Interpreter}
    
    -- instance (Monad IOI) where
    
    
    newInterpreter :: Interpreter
    newInterpreter = (M.empty, M.empty)
    
    
    x <::> y = y <$> x
    
    main :: IO ()
    main = do
      ppt "x = 7" newInterpreter 
        >>= ppt "x + 6"
        >>= ppt "x + 6" 
        >>= ppt "y + 7" 
        >>= ppt "x = 13 + (y = 3)" 
        >>= ppt "x = y = 7" 
        >>= ppt "x = 13 + (y = 3)"
        >>= ppt "fn avg x y => (x + y) / 2"
        >>= ppt "avg 1 2"
        >>= ppt "a = 6"
        >>= ppt "b = 7"
        >>= ppt "avg a b"
      -- -- ppt "add echo 4 echo 3"
      -- i <- ppt "fn inc x => x + 2.0 / 1" i
      -- ppt expr1
      -- print r
      return ()