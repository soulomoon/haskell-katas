{-# LANGUAGE LambdaCase #-}


module Kyu2.Lisp where

import           Text.ParserCombinators.Parsec
                                         hiding ( parens )
import           Control.Monad
import           Data.Maybe
import           Data.List
import           Data.Monoid


data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)

space1 :: Parser String
space1 = many1 space

parens :: Parser AST -> Parser AST
parens m = do
  char '('
  spaces
  n <- m
  spaces
  char ')'
  return n

numbers :: Parser AST
numbers = do
  ns <- many1 digit
  return $ I32 (read ns :: Int)

lists :: Parser AST
lists = Lst <$> many (try exprs1)

nodes :: Parser AST
nodes = parens $ do
  op       <- exprs
  Lst args <- lists
  return $ Nod op args

nulls :: Parser AST
nulls = parens (return Nul) <|> (string "null" >> return Nul)

booleans :: Parser AST
booleans =
  (string "true" >> return (Boo True))
    <|> (string "false" >> return (Boo False))

symbols :: Parser AST
symbols = do
  c    <- noneOf $ " ,\n\t\r()" ++ ['0' .. '9']
  rest <- many $ noneOf " ,\n\t\r()"
  return $ Sym $ c : rest

exprs1 :: Parser AST
exprs1 = try (space1 >> exprs) <|> nodes

exprs :: Parser AST
exprs =
  spaces >> (try booleans <|> try nulls <|> symbols <|> numbers <|> nodes)


type PreludeFunc = [AST] -> AST
type BinaryFunc = AST -> AST -> AST

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+"      , lenWrapLg 1 $ i32ManyI (+))
  , ("*"      , lenWrapLg 1 $ i32ManyI (*))
  , ("-"      , lenWrapLg 2 $ i32ManyI (-))
  , ("/"      , lenWrapLg 2 i32div)
  , ("^"      , lenWrap 2 2 $ i32ManyI (^))
  , (">"      , i32ManyB (>))
  , ("<"      , i32ManyB (<))
  , ("!"      , \case [Boo x] -> Boo $ not x;_->Err)
  , ("list"   , Lst)
  , ("size"   , \case [Lst xs] -> I32 $ length xs;_->Err)
  , ("reverse", \case [Lst xs]-> Lst $ reverse xs;_->Err)
  , (".."     , \case [I32 a, I32 b] -> Lst $ map I32 [a .. b];_->Err)
  , ("=="     , lenWrapLg 2 $ \xs -> Boo $ all (== head xs) xs)
  , (">="     , i32ManyB (>=))
  , ("<="     , i32ManyB (<=))
  , ("!="     , i32ManyB (/=))
  , ("if"     , \case (Boo x:xs)->(if x then head else head.init) (xs++[Nul, Err]);_->Err)
  ]
 where
  i32ManyB f [I32 x, I32 y] = Boo $ f x y
  i32ManyB f _              = Err

  i32BinaryI :: (Int -> Int -> Int) -> BinaryFunc
  i32BinaryI f (I32 x) (I32 y) = I32 $ f x y
  i32BinaryI f _       _       = Err

  i32ManyI :: (Int -> Int -> Int) -> PreludeFunc
  i32ManyI f [x         ] = x
  i32ManyI f (x : y : xs) = i32ManyI f $ i32BinaryI f x y : xs

  i32div (I32 x : I32 y : xs) | otherwise = i32div $ (I32 $ div x y) : xs
                              | y == 0    = Err
  i32div [x] = x
  i32div _   = Err

  lenWrap :: Int -> Int -> PreludeFunc -> PreludeFunc
  lenWrap lower upper f args | (len >= lower) && (len <= upper) = f args
                             | otherwise                        = Err
    where len = length args
  lenWrapLg :: Int -> PreludeFunc -> PreludeFunc
  lenWrapLg l = lenWrap l maxBound



parseF :: (AST -> Maybe a) -> String -> Maybe a
parseF f s = case parse exprs "(unknown)" s of
  Right r -> f r
  _       -> Nothing


lispShow :: String -> Maybe String
lispShow = parseF (Just . show)

lispPretty :: String -> Maybe String
lispPretty = parseF prettyE
 where
  prettyE :: AST -> Maybe String
  prettyE (Nod e xs)  = wrapParens . unwords <$> mapM prettyE (e : xs)
  prettyE Nul         = Just "null"
  prettyE Err         = Nothing
  prettyE (Lst xs   ) = unwords <$> mapM prettyE xs
  prettyE (Boo False) = Just "false"
  prettyE (Boo True ) = Just "true"
  prettyE (Sym s    ) = Just s
  prettyE (I32 s    ) = Just $ show s
  wrapParens s = "(" ++ s ++ ")"

lispEval :: String -> Maybe AST
lispEval = parseF evalE
 where
  evalE :: AST -> Maybe AST
  evalE (Nod (Sym s) xs) = getAlt $ foldMap f preludeFunctions `mplus` Alt (Just Err)
   where
    f :: (String, [AST] -> AST) -> Alt Maybe AST
    f (t, func) | t == s    = func <$> mapM (Alt . evalE) xs
                | otherwise = Alt Nothing
  evalE (Nod _ _) = Nothing
  evalE a         = Just a

  