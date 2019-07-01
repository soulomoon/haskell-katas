module Kyu2.SimpleSQLEngine (sqlEngine) where

import           Text.Parsec
import qualified Text.Parsec.Token             as Tok
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char)
import Data.Functor.Identity
import Control.Monad
import Debug.Trace
import Data.Char
import Data.List
import Data.Either
import Data.Maybe (fromMaybe)

reservedNames = ["select", "from", "where", ".", "on" ]
reservedOps = [ "=", "<=", ">=", ">", "<", "<>"]

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { 
    Tok.commentStart = ""
  , Tok.commentEnd = ""
  , Tok.commentLine = ""
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":"
  , Tok.opLetter        = oneOf ":"
  , Tok.reservedNames   = reservedNames 
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive   = True
  }
type TableName = String
type ColumnName = String
type Select = [ColumnId]
type From = TableName 
data Query = Query Select From [Join] [Where]
  deriving Show
data Join = Join TableName ValueTest
  deriving Show
data ValueTest = ValueTest Value Comparison Value
  deriving Show
type Where = ValueTest
data Value = ValueC ColumnId | ValueT Const
  deriving Show
-- type ColumnId = (TableName, ColumnName)
type ColumnId = String
type Const = String
data Comparison = Lt | Eq | Gt | Lte | Gte | Ueq
  deriving Show
 
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
number = Tok.integer lexer
symbol = Tok.symbol lexer
identifier = Tok.identifier lexer
parens = Tok.parens lexer
stringLiteral = Tok.stringLiteral lexer


comparison :: Parser Comparison
comparison = 
  try (reservedOp "<>" >> return Ueq) <|>
  (reservedOp "=" >> return Eq) <|>
  try (reservedOp "<=" >> return Lte) <|>
  try (reservedOp ">=" >> return Gte) <|>
  (reservedOp ">" >> return Gt) <|>
  (reservedOp "<" >> return Lt) 

quotations = Tok.lexeme lexer . between (symbol "'") (symbol "'")

escape :: Parser String
escape = do
    d <- string "''"
    return "'"

nonEscape :: Parser Char
nonEscape = noneOf "'"

character :: Parser String
character = fmap return nonEscape <|> try escape 

constv :: Parser Const
constv = (show <$> number) <|> 
  quotations (trim . concat <$> many character)

trim :: String -> String
trim = map (\c -> if c == '\n' then ' ' else c) . dropWhileEnd isSpace . dropWhile isSpace

wheret :: Parser Where
wheret = reserved "where" >> valueTest

value :: Parser Value
value = ValueC <$> columnId <|> ValueT <$> constv

valueTest :: Parser ValueTest
valueTest = liftM3 ValueTest value comparison value

joint :: Parser Join
joint = do
  reserved "join"
  tableName <- identifier
  reserved "on"
  Join tableName <$> valueTest

query :: Parser Query
query = do
  s <- select
  f <- from
  js <- many joint
  ws <- many wheret
  return $ Query s f js ws


term :: Parser ()
term = reserved "if"

columnId :: Parser ColumnId
columnId = do
  tableName <- identifier
  string "."
  columnName <- identifier 
  return (tableName ++ "."++ columnName)

select :: Parser Select
select = do
  reserved "select"
  sepBy1 columnId (symbol ",")

from :: Parser From
from = do
  reserved "from"
  identifier

parseTerm :: String -> Either ParseError Query
parseTerm = parse query "name" . traceShowId . trim . map toLower

ppt :: String -> IO ()
ppt s = do 
  putStrLn "------"
  print $ parseTerm s

type DataBase = [(TableName,Table)]
type Table = [[(String,String)]]
type QueryResult = [[(String,String)]]

getTable :: TableName -> DataBase -> Table
getTable tableName = snd . head . filter ((==tableName) . map toLower . fst)


getValue :: [(String, String)] -> Value -> Const
getValue row (ValueC id) = fromMaybe (error "getValue error") (lookup id $ map (\(a, b) -> (map toLower a, b)) row)
getValue row (ValueT constv) = constv


passValueTest (ValueTest val0 cmp val1) row = 
  case cmp of 
    Eq -> v0 == v1
    Ueq -> v0 /= v1
    Gt -> v0 > v1
    Lt -> v0 < v1
    Gte -> v0 >= v1
    Lte -> v0 <= v1
  where 
    v0 = map toLower $ getValue row val0 
    v1 = map toLower $ getValue row val1

selectResult :: Select -> QueryResult -> QueryResult
-- selectResult s = map (filter (flip elem s . fst))
selectResult ids = map (\row -> map (\id -> (id, getValue row (ValueC id))) ids)
showColumnId (name, col) = name ++ "." ++ col

sqlEngine :: DataBase -> String -> QueryResult
sqlEngine database = execute where
  execute :: String -> [[(String,String)]]
  execute query = run qry
    where 
      run :: Query -> [[(String,String)]]
      run (Query s f js vs) = selectResult s $ 
        filter (\row -> all (`passValueTest` row) vs) fd
        where 
          fd = foldl joinTable (getTable f hData) js
      joinTable :: QueryResult -> Join -> QueryResult
      joinTable rows (Join tableName valuetest) = filter (passValueTest valuetest) allRows
        where newRows = getTable tableName hData
              allRows = [r0 ++ r1 | r0 <- rows, r1 <- newRows]
      hData :: [(String,[[(String,String)]])] 
      hData = map (\(name, rows)-> (name, map (map (\(col, value) -> (name ++ "." ++ col, value))) rows)) database
      qry :: Query
      qry = case traceShowId $ parseTerm query of
                 Right q -> q
                 Left q -> error "parse error"

movieDatabase = [ ( "movie"
                  , [ [ ( "id", "1" ), ( "name", "Avatar"   ), ( "directorID", "1" ) ]
                    , [ ( "id", "2" ), ( "name", "Titanic"  ), ( "directorID", "1" ) ]
                    , [ ( "id", "3" ), ( "name", "Infamous" ), ( "directorID", "2" ) ]
                    , [ ( "id", "4" ), ( "name", "Skyfall"  ), ( "directorID", "3" ) ]
                    , [ ( "id", "5" ), ( "name", "Aliens"   ), ( "directorID", "1" ) ]
                    ]
                  )
                , ( "actor"
                  , [ [ ( "id", "1" ), ( "name", "Leonardo DiCaprio" ) ]
                    , [ ( "id", "2" ), ( "name", "Sigourney Weaver"  ) ]
                    , [ ( "id", "3" ), ( "name", "Daniel Craig"      ) ]
                    ]
                  )
                , ( "director"
                  , [ [ ( "id", "1" ), ( "name", "James Cameron"   ) ]
                    , [ ( "id", "2" ), ( "name", "Douglas McGrath" ) ]
                    , [ ( "id", "3" ), ( "name", "Sam Mendes"      ) ]
                    ]
                  )
                , ( "actor_to_movie"
                  , [ [ ( "movieID", "1" ), ( "actorID", "2" ) ]
                    , [ ( "movieID", "2" ), ( "actorID", "1" ) ]
                    , [ ( "movieID", "3" ), ( "actorID", "2" ) ]
                    , [ ( "movieID", "3" ), ( "actorID", "3" ) ]
                    , [ ( "movieID", "4" ), ( "actorID", "3" ) ]
                    , [ ( "movieID", "5" ), ( "actorID", "2" ) ]
                    ]
                  )
                ]


main :: IO ()
main = do
  -- ppt "select table_d.id from table \
  --   \join table2 on table1.id > table.id \
  --   \join table2 on table1.id > table.id\
  --   \where movie.name = table.id"
  -- ppt "select movie.name from movie"
  -- ppt "SELECT movie.name FROM movie WHERE movie.directorID = '1 '"
  -- ppt "Select movie.name, director.name From movie Join director On director.id = movie.directorID"
  -- ppt "SelecT movie.name,director.name\nFroM director\nJoiN movie ON director.id = movie.directorID\n"
  -- ppt "select movie.name   \
  --   \     , actor.name\
  --   \  FROM movie\
  --   \  Join actor_to_movie\
  --   \       oN actor_to_movie.movieID=movie.id\
  --   \  JoIn actor\
  --   \    oN actor_to_movie.actorID = actor.id\
  --   \ WheRe \n\
  --   \   actor.name <> 'Daniel Craig'"
  print $ sqlEngine movieDatabase "select movie.name from movie where movie.name='Titanic'"
  print $ parseTerm "select movie.title from movie where movie.title > 'pirates of the caribbean: dead man''s chest'"
  return ()

