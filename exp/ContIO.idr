module Main

import Data.IORef

%default total

--------------------------------------------------------------------------------

-- Cont monad

data Cont r a
  = MkCont ((a -> r) -> r)

runCont : (a -> r) -> Cont r a -> r
runCont k (MkCont f) = f k

Functor (Cont r) where
  map f (MkCont g) = MkCont (\k => g (k . f))

Applicative (Cont r) where
  pure x = MkCont (\k => k x)
  (<*>) (MkCont f) (MkCont x) = MkCont (\k => f (\f' => x (\x' => k (f' x'))))

Monad (Cont r) where
  (>>=) (MkCont x) f = MkCont (\k => x (\x' => runCont k (f x')))

-- Our magic trick!

lift : Monad m => m a -> (Cont (m b) a)
lift = MkCont . (>>=)

--------------------------------------------------------------------------------
-------------------------------------- IO --------------------------------------
--------------------------------------------------------------------------------

-- IO on Cont

interface MonadIO (m: Type -> Type) where
  liftIO : IO a -> m a

MonadIO IO where
  liftIO = id

(Monad m, MonadIO m) => MonadIO (Cont (m b)) where
  liftIO = MkCont . (>>=) . liftIO

print : (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.Interactive.print

printLn : (MonadIO m, Show a) => a -> m ()
printLn = liftIO . Prelude.Interactive.printLn

putStr : MonadIO m => String -> m ()
putStr = liftIO . Prelude.Interactive.putStr

putStrLn : MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.Interactive.putStrLn

{-
--------------------------------------------------------------------------------
---------------------------------- BACKTRACKING --------------------------------
--------------------------------------------------------------------------------

-- This is kinda a hack... but the instances are lawful. Not sure why these
-- aren't in the stdlib.

Semigroup () where () <+> () = ()
Monoid () where neutral = ()

-- Lazy Lists; the builtin Stream type is infinite. With strict lists this
-- still works, it's just very slow.

data LazyList : Type -> Type where
  Nil : LazyList a
  (::) : a -> Lazy (LazyList a) -> LazyList a

Functor LazyList where
  map f [] = []
  map f (x::xs) = f x :: map f xs

Foldable LazyList where
  foldr _    nil [] = nil
  foldr cons nil (x::xs) = cons x (foldr cons nil xs)

filter : (a -> Bool) -> LazyList a -> LazyList a
filter f [] = []
filter f (x::xs) = if f x then x :: filter f xs else filter f xs

-- Backtracking on Cont

Monoid m => Alternative (Cont m) where
  empty = MkCont (\_ => pure neutral)
  (<|>) (MkCont l) (MkCont r) = MkCont (\k => (<+>) <$> (l k) <*> (r k))

fail : Monoid m => Cont m a
fail = empty

choose : Monoid m => LazyList a -> Cont m a
choose [] = fail
choose (x::xs) = pure x <|> choose xs

ensure : Monoid m => Bool -> Cont m ()
ensure True = pure ()
ensure False = fail

-- Perfect number finder

rangeHelper : Nat -> Nat -> LazyList Nat
rangeHelper _ Z = []
rangeHelper x (S y) = x :: rangeHelper (S x) y

range : Nat -> Nat -> LazyList Nat
range lo hi = rangeHelper lo (minus hi lo)

isDivisorPred : Nat -> Nat -> Bool
isDivisorPred a predB = modNatNZ a (S predB) SIsNotZ == 0

divisorsOf : Nat -> LazyList Nat
divisorsOf Z = []
divisorsOf (S predN) = map S $ filter (isDivisorPred (S predN)) (range 0 predN)

findPerfectNumberBelow : Monoid m => Nat -> Cont m Nat
findPerfectNumberBelow Z = fail
findPerfectNumberBelow (S predMax) = do
  n <- choose (range 1 predMax)
  ensure (n == sum (divisorsOf n))
  pure n

doPerfectNumberFinder : Monoid m => Cont m ()
doPerfectNumberFinder = do
  putStrLn "Searching for perfect numbers below 1000 (should find 6, 28, and 496)."
  n <- findPerfectNumberBelow 1000
  putStrLn ("Found " ++ show n ++ "!")
  pure ()

--------------------------------------------------------------------------------
------------------------------------- STATE ------------------------------------
--------------------------------------------------------------------------------

State' : Type -> Type -> Type
State' s a = s -> IO (a, s)

get : State' s s
get s = pure (s, s)

stateBind : State' s a -> (a -> State' s b) -> State' s b
stateBind m f s = (m s) >>= uncurry f

get' : (s -> State' s b) -> State' s b
get' k = stateBind get k

--------------------------------------------------------------------------------

main : IO ()
main = do
  runCont' doPerfectNumberFinder
  -}
