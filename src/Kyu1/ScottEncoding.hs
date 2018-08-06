{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module Kyu1.ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toEither :: SEither a b -> Either a b
toEither seab = runEither seab Left Right
fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither $ \ac bc -> ac a 
fromEither (Right b) = SEither $ \ac bc -> bc b
isLeft :: SEither a b -> Bool
isLeft e = case toEither e of
             Left _ -> True
             Right _ -> False
isRight :: SEither a b -> Bool
isRight = not . isLeft
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = fromPair . foldr (\a (lista, listb)-> case toEither a of 
                                                         Left a -> (cons a lista, listb)
                                                         Right b -> (lista, cons b listb)) 
                                  (SList const, SList const)


toList :: SList a -> [a]
toList mb = runList mb [] (\x rest -> x:toList rest)
fromList :: [a] -> SList a
fromList [] = SList const
fromList (x:xs)= SList $ \_ f -> f x (fromList xs) 
cons :: a -> SList a -> SList a
cons a list = SList $ \_ f -> f a list
concat :: SList a -> SList a -> SList a
concat x y | null x = y
  | otherwise = runList x (SList const) (\r rest -> cons r $ concat rest y) 
null :: SList a -> Bool
null mb = case toList mb of 
            [] -> True 
            x:_ -> False
length :: SList a -> Int
length mb = runList mb 0 (\ x rest -> 1 + length rest)
map :: (a -> b) -> SList a -> SList b
map g mb | null mb = SList const
         | otherwise = runList mb (SList const) (\x rest -> cons (g x) $ map g rest)
zip :: SList a -> SList b -> SList (SPair a b)
zip ma mb = case (toList ma, toList mb) of
              (x:xs, y:ys) -> cons (fromPair (x, y)) $ zip (fromList xs) (fromList ys)
              _ -> SList const
              

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f b l = runList l b (\x rest -> foldl f (f b x) rest) 
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f b l = runList l b (\x rest -> f x (foldr f b rest))
take :: Int -> SList a -> SList a
take m list = runList list (SList const) (\x rest -> case m of 
                                                       0 -> SList const
                                                       _ -> cons x $ take (m-1) rest)

toMaybe :: SMaybe a -> Maybe a
toMaybe mb = runMaybe mb Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe (Just a) = SMaybe $ \b f -> f a
fromMaybe Nothing = SMaybe const
isJust :: SMaybe a -> Bool
isJust mb = case toMaybe mb of 
              Just _ -> True 
              Nothing -> False
isNothing :: SMaybe a -> Bool
isNothing = not . isJust
catMaybes :: SList (SMaybe a) -> SList a
catMaybes = foldr (\x rest -> case toMaybe x of 
                                Nothing -> rest
                                Just x -> cons x rest)
                  (SList const)


toPair :: SPair a b -> (a,b)
toPair (SPair sp) = sp (\a b -> (a, b))
fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair (\f -> f a b) 
fst :: SPair a b -> a
fst sp = runPair sp const
snd :: SPair a b -> b
snd sp = runPair sp (\_ b -> b)
swap :: SPair a b -> SPair b a
swap sp = fromPair (snd sp, fst sp)
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f $ fromPair (a, b)
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f sp = f (fst sp) (snd sp)
