module Kyu3.Isomorphism where

import Data.Void
import Data.Tuple
import Data.Maybe
-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm = swap

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans p d = (fst d . fst p ,  snd p . snd d)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList x = (fmap $ fst x, fmap $ snd x)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe x = (fmap $ fst x, fmap $ snd x)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (ff, gg) where
    ff (Left a) = Left (ab a)
    ff (Right c) = Right (cd c)
    gg (Left b) = Left (ba b)
    gg (Right d) = Right (dc d)


isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (f, g) where 
    f ac = cd . ac . ba
    g bd = dc . bd . ab

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (abM, baM) = (ab, ba) where
    ab a =  case abM (Just a) of 
                 Just c -> c
                 nothing -> fromJust $ abM Nothing
    ba b = case baM (Just b) of
                 Just d -> d
                 nothing -> fromJust $ baM Nothing
-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.

-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (f, g) where 
    f (Left n) = Left $ ():n
    f (Right _) = Left []
    g (Left (_:r)) = Left r
    g (Left _) = Right ()
-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (swap, swap)
