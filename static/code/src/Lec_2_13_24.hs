{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lec_2_13_24 where

import Prelude

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + (toInt n)


-- dec :: Nat -> Nat
dec :: Nat -> Nat
dec Zero = Zero
dec (Succ n) = n

{- (A) `n`
   (B) something else
-}

one :: Nat
one = Succ Zero

two :: Nat
two = Succ one

two' :: Nat
two' = Succ (Succ Zero)

fromInt :: Int -> Nat
fromInt i = if i <= 0 then Zero else Succ (fromInt (i - 1))

{-
foo 2
==> Succ (foo 1)
==> Succ (Succ (foo 0))
==> Succ (Succ Zero)
-}

-- >>> two
-- Succ (Succ Zero)

-- >>> dec (Succ Zero) == Zero
-- True

-- >>> dec (Succ (Succ Zero)) == (Succ Zero)
-- True

-- >>> dec (Succ (Succ (Succ Zero))) == Succ (Succ Zero)
-- True

-- >>> addHack (Succ Zero) (Succ (Succ Zero))
-- Succ (Succ (Succ Zero))

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)
-- add (Succ n)    m        = add n (Succ m)




{-

add (Succ (Succ Zero))  (Succ (Succ (Succ Zero)))

(Succ (Succ (Succ (Succ (Succ Zero)))))






add (Succ (Succ Zero))  (Succ (Succ (Succ Zero)))
==>
Succ (add (Succ Zero) (Succ (Succ (Succ Zero))))
==>
Succ (Succ (add Zero (Succ (Succ (Succ Zero))) ))
==>
Succ (Succ ( (Succ (Succ (Succ Zero))) ))



add (Succ Zero) Zero
==> (Succ Zero)


add (Succ Zero) Zero
==>
add Zero (Succ Zero)
==>
Succ Zero




add (Succ (Succ Zero))  (Succ (Succ (Succ Zero)))
==>
add (Succ Zero) (Succ(Succ (Succ (Succ Zero))))
==>
add Zero     (Succ(Succ(Succ (Succ (Succ Zero)))))
==>
Succ(Succ(Succ(Succ (Succ Zero))))





-}


-- add Zero        (Succ Zero)        = Succ Zero
-- add Zero        (Succ (Succ Zero)) = Succ (Succ Zero)

-- add (Succ Zero) Zero        = Succ Zero
-- add (Succ Zero) (Succ Zero) = Succ (Succ Zero)


sub :: Nat -> Nat -> Nat
sub Zero      _        = Zero
sub n         Zero     = n
sub (Succ n)  (Succ m) = sub n m


addHack :: Nat -> Nat -> Nat
addHack n m = fromInt ((toInt n) + (toInt m))

data Op = Add | Sub | Mul

-- type Op = Char

data Expr
  = MkNum Double
  | MkBin Op Expr Expr

  -- | MkAdd Expr Expr
  -- | MkSub Expr Expr
  -- | MkMul Expr Expr

  -- (A) | MkAdd Double Double
  -- (B) | MkAdd Expr   Expr

-- >>> (1.1 + 2.2) * (3.3 - 4.4)
-- -3.630000000000002

-- >>> eval exp0
-- -3.630000000000002

exp0 :: Expr
exp0 = MkBin Mul
        (MkBin Add (MkNum 1.1) (MkNum 2.2))
        (MkBin Sub (MkNum 3.3) (MkNum 4.4))

eval :: Expr -> Double
eval (MkNum n) = n
eval (MkBin Add e1 e2) =
  let
    v1 = eval e1
    v2 = eval e2
  in
    v1 + v2

eval (MkBin Sub e1 e2) =
  let
    v1 = eval e1
    v2 = eval e2
  in
    v1 - v2


eval (MkBin Mul e1 e2) =
  let
    v1 = eval e1
    v2 = eval e2
  in
    v1 * v2




data List s = Nil | Cons s (List s)

list0 :: List String
list0 = Cons "1" (Cons "2"  (Cons "3" Nil))

-- >>> size list0
-- 3

size :: List a -> Int
size Nil = 0
size (Cons _ t) =

  let n = size t
  in
    1 + n
