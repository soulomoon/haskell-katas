{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Kyu1.Ski where

-- Formally, in lambda calculus, SKI combinators are defined as:
-- Sxyz = xz(yz)
-- Kxy = x
-- Ix = x
-- Where every term is applied to the previous term left-associatively, e.g abcd = ((ab)c)d.
-- (Note that I can be defined in terms of S and K. It's usually kept mostly for convenience.)
-- 
-- I and K should be familiar to you as `id` and `const` in Haskell, but S is a bit complicated.
-- Formally, S represents term application: it accepts 3 arguments `x`, `y`, `z`,
-- which then we apply to `x` two arguments: first `z`, then `yz`, i.e the result of `z` applied to `y`.
-- (Everything in lambda calculus are terms. There are no distinctions between values and functions.)
-- 
-- Sure, you can use SKI combinators in Haskell all you want, but don't forget that this is Haskell ;-)
-- With a little effort (a.k.a GADT), SKI can be encoded as an AST in Haskell.
-- Encoding SKI as an AST allows the typechecker to verify AST type correctness for us.
-- (It also prevents us from using non-SKI things to cheat the system ;-))
-- Note that we also need `Ap`, which applies one AST to another at the type level,
-- to provide ourselves a way to transform our AST.

------------------------------------------------------------------------------
-- Task #1: Read SKI's data type (pre)defined as below, and understand what's going on ;-)

data SKI :: * -> * where
   Ap :: SKI (a -> b) -> SKI a -> SKI b
   S :: SKI ((a -> b -> c) -> (a -> b) -> a -> c)
   K :: SKI (a -> b -> a)
   I :: SKI (a -> a)


-- If we also have:  Var :: a -> SKI a , then it's just a normal DSL.
-- It will also automatically give us the Functor and Applicative instances.
-- However, the point of combinator calculus (or lambda calculus in general)
-- is to operate on combinators and terms themselves, and so there are no ways
-- (and will never have any) to inject values directly.
-- 
-- If there are no ways to inject values directly, then how do we do things with them?
-- The answer is simple: the combinators change how a term is applied to its arguments.
-- This allows us to express any lambda term in terms of these combinators.
-- This is Church's thesis: Any suitable combinator basis (e.g SK/SKI) can form all kinds (as in Haskell's Kind) of computable functions.

------------------------------------------------------------------------------
-- Task #2: implement the evaluator and pretty-printer for the SKI system.

evalSKI :: SKI a -> a
evalSKI I = id
evalSKI K = const
evalSKI S = \abc ab a -> abc a (ab a)
evalSKI (Ap skab ska) = evalSKI skab $ evalSKI ska

-- The pretty-printer should follow this format:
-- I, K, S -> "I", "K", "S"
-- Ap a b -> "(a b)" where a and b are subterms
prettyPrintSKI :: SKI a -> String
prettyPrintSKI I = "I"
prettyPrintSKI K = "K"
prettyPrintSKI S = "S"
prettyPrintSKI (Ap skab ska) = "(" ++ prettyPrintSKI skab ++ " " ++ prettyPrintSKI ska ++ ")"


------------------------------------------------------------------------------
-- Task #3: write the following basic combinators in the SKI system.

-- Transforming a given lambda term to a combination of combinators is basically making
-- your code point-free in Haskell: You write \x y -> x (lambda function) into `const` (point-free).
-- To distinguish combinators (sort-of functions) from parameters (what we want to get rid of eventually),
-- we use uppercase letters for combinators and lowercase letters for parameters(and whitespace to separate each term).
-- In order to do this, we can add or remove extra parameters *at the end* of an expression (Eta reduction).

-- e.g: By definition,
--    I
-- => \x. x          -- apply I
-- => \x. K x _      -- replace x with K x _
-- => \x. K x (_ x)  -- substitute _ with _ x (_ is just a dummy term which can be anything)
-- => \x. S K _ x    -- condense S
-- => S K _          -- eta reduction
-- QED

-- The proof goes to the other way too (all operations above are bijective),
-- i.e this inverse of the above proof automatically true too, as below:
--    S K _
-- => \x. S K _ x    -- eta reduction
-- => \x. K x (_ x)  -- apply S
-- => \x. x          -- apply K
-- => I              -- condense I
-- QED

-- Hence this gives us the proof that S K _ = I, i.e I can be expressed by S and K.

-- After finishing our hand-written proof, by Curry-Howard correspondence,
-- we can always encode our initial lambda term into a type (as long as no recursion (e.g Y) happens).
-- This allows us to check for proof correctness via type-checker. How convenient!

-- As can be seen above, expanding everything into S and K only is long and tedious.
-- Any simple lambda might give rise to a proof of dozen of lines:
--    \x. \y. \z. x z y
-- => \x. \y. \z. (x z) (K y z)   -- replace y with K y z
-- => \x. \y. \z. (x z) ((K y) z)
-- => \x. \y. \z. S x (K y) z     -- condense S
-- => \x. \y. S x (K y)           -- eta reduction
-- => \x. \y. (S x) (K y)
-- => \x. \y. (K (S x) y) (K y)   -- replace S x with K (S x) y
-- => \x. \y. (K (S x)) y (K y)
-- => \x. \y. S (K (S x)) K y     -- condense S
-- => \x. S (K (S x)) K           -- eta reduction
-- ...

-- In fact, there are infinitely many ways to express any lambda term,
-- with no computationally easy way to find the shortest one.
-- That's why we instead define lots of other combinators to help us compose new combinators
-- without expanding into a long chain of S and K (which are known to grow non-linearly).
-- You should do that. Break a proof into parts to reduce the mental workload.
rev1 :: SKI (a -> a)
rev1 = Ap (Ap S K) K

rev :: SKI (a -> (a -> b) -> b)
rev = Ap (Ap S $ Ap K $ Ap S I) K

comp :: SKI ((b -> c) -> (a -> b) -> (a -> c))
comp = Ap (Ap S (Ap K S)) K

flip' :: SKI ((a -> b -> c) -> (b -> a -> c))
flip' = Ap (Ap comp (Ap (Ap comp (Ap rev K)) comp)) S

rotr :: SKI (a -> (c -> a -> b) -> c -> b)
rotr = Ap (Ap comp comp) rev

rotv :: SKI (a -> b -> (a -> b -> c) -> c)
rotv = Ap (Ap comp (Ap (Ap flip' comp) rotr)) rotr

-- We can't write `fix` i.e Y in Haskell because Haskell is typed
-- (well, at least without recursive types), but we can still write `join`
join :: SKI ((a -> a -> b) -> a -> b)
join = Ap (Ap flip' S) I

------------------------------------------------------------------------------
-- Task #4: implement Boolean algebra in the SKI system

-- Boolean algebra is represented as an if-else statement:
-- T accepts two arguments, and returns the first argument.
-- F also accepts two arguments, but returns the second instead.
--
-- Note: all the operators should be prefix. They should also be lazy,
-- which should come along naturally if you're doing it correctly.

-- type synonym to help reduce clutter in type definition
type Bool' a = a -> a -> a

-- Note: The correct type of everything down there should be something along
-- (forall a. Bool a) instead of (Bool a)
-- However, we cannot express it in SKI because it is embedded here as
-- simply typed lambda calculus (STLC), which does not have enough expressive power for this.
-- (In other systems, e.g untyped lambda calculus (UTLC) and System F, we don't have this problem.)
-- As a result, your term can be correct even when the type-checking cannot deduce such.
-- If you're absolutely ensure that your proof is correct, you can remove the type annotation,
-- or rewrite its corresponding SKI type.

true :: SKI (Bool' a)
true = K

false :: SKI (Bool' a)
false = Ap flip' true

not' :: SKI (Bool' a -> Bool' a)
not' = flip'

and' :: SKI (Bool' (Bool' a) -> Bool' a -> Bool' a)
and' = Ap rotr false

or' :: SKI (Bool' (Bool' a) -> Bool' a -> Bool' a)
or' = Ap rev K

-- xor' :: SKI (Bool' (Bool' a -> Bool' a) -> Bool' a -> Bool' a)
xor' = Ap (Ap comp (Ap comp not')) (Ap (Ap flip' S) not')
