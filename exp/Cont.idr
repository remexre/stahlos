module Main

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

--------------------------------------------------------------------------------
------------------------------------- STATE ------------------------------------
--------------------------------------------------------------------------------

get : Cont (s -> (b, s)) s
get = MkCont (\g, s => g s s)

put : s -> Cont (s -> (b, s)) ()
put x = MkCont (\g, s => g () x)

modify : (s -> s) -> Cont (s -> (b, s)) ()
modify f = get >>= put . f

doDoubleAdd2 : Cont (Int -> (b, Int)) ()
doDoubleAdd2 = do
  modify (*2)
  modify (+2)

--------------------------------------------------------------------------------
---------------------------------- BACKTRACKING --------------------------------
--------------------------------------------------------------------------------

-- This is kinda a hack... but the instances are lawful. Not sure why these
-- aren't in the stdlib.

Semigroup () where () <+> () = ()
Monoid () where neutral = ()
-- The fully general forms of these (the instances work for any applicative)
-- overlap with Maybe's instances, and for some reason I can't use named
-- instances.
Semigroup a => Semigroup (IO a) where (<+>) = liftA2 (<+>)
Monoid a => Monoid (IO a) where neutral = pure neutral

-- Lazy Lists; the builtin Stream type is infinite. With strict lists this
-- still works, it's just very slow.

data LazyList : Type -> Type where
  Nil : LazyList a
  (::) : a -> Lazy (LazyList a) -> LazyList a

Semigroup (LazyList a) where
  (<+>) []      y = y
  (<+>) (x::xs) y = x :: (xs <+> y)

Monoid (LazyList a) where
  neutral = []

Functor LazyList where
  map f [] = []
  map f (x::xs) = f x :: map f xs

Foldable LazyList where
  foldr _    nil [] = nil
  foldr cons nil (x::xs) = cons x (foldr cons nil xs)

filter : (a -> Bool) -> LazyList a -> LazyList a
filter f [] = []
filter f (x::xs) = if f x then x :: filter f xs else filter f xs

toList : LazyList a -> List a
toList [] = []
toList (x::xs) = x :: Main.toList xs

-- Backtracking on Cont

Monoid m => Alternative (Cont m) where
  empty = MkCont (\_ => neutral)
  (<|>) (MkCont l) (MkCont r) = MkCont (\k => l k <+> r k)

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

--------------------------------------------------------------------------------

main : IO ()
main = do
  putStr "12 = "
  printLn (snd (runCont (\_, s => ((), s)) doDoubleAdd2 5))
  putStrLn "Searching for perfect numbers below 1000 (should find 6, 28, and 496)."
  printLn (toList (runCont (\x => [x]) (findPerfectNumberBelow 1000)))
