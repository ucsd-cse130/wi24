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




data List a = Nil | Cons a (List a) deriving (Show)

list0 :: List String
list0 = Cons "1" (Cons "2"  (Cons "3" Nil))


-- >>> append Nil (Cons 3 (Cons 4 (Cons 5 Nil)))
-- Cons 3 (Cons 4 (Cons 5 Nil))

-- >>> append (Cons 2 Nil) (Cons 3 (Cons 4 (Cons 5 Nil)))
-- Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))

{-
  append (Cons 2 Nil) ys
  ==>
  Cons 2 (append Nil ys)
  ==>
  Cons 2 ys

  append (Cons 1 (Cons 2 Nil)) ys
  ==>
  Cons 1 (append (Cons 2 Nil) ys)
  ==>
  Cons 1 (Cons 2 ys)


  append (Cons x1 (Cons x2 (Cons x3 Nil)))  y_50_bill
  ==>
  Cons x1 (append ((Cons x2 (Cons x3 Nil)))  y_50_bill)
  ==>
  Cons x1 (Cons x2 (( append (((Cons x3 Nil)))  y_50_bill))
  ==>
  Cons x1 (Cons x2 (Cons x3 (append Nil  y_50_bill)))
  ==>
  Cons x1 (Cons x2 (Cons x3 y_50_bill))

-}

-- >>> rev (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons 3 (Cons 2 (Cons 1 Nil))

{-
  rev (Cons 1 (Cons 2 (Cons 3 Nil)))
==>
  append (rev (Cons 2 (Cons 3 Nil))) (Cons 1 Nil)
==>
  append (append (rev (Cons 3 Nil)) (Cons 2 Nil)) (Cons 1 Nil)
==>
  append (append (append (rev Nil) (Cons 3 Nil)) (Cons 2 Nil)) (Cons 1 Nil)
==>
  append (append (append (Nil) (Cons 3 Nil)) (Cons 2 Nil)) (Cons 1 Nil)
==>
  ((Nil +++ (Cons 3 Nil)) +++ (Cons 2 Nil)) +++ (Cons 1 Nil)
==>
  Cons 3 (Cons 2 (Cons 1 Nil))



-}

-- >>> Nil +++ (Cons 4 Nil)
-- Cons 4 Nil

-- >>> ((Cons 4 Nil)) +++ (Cons 3 Nil)
-- Cons 4 (Cons 3 Nil)


-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
-- >>> append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 (Cons 5 Nil)))
-- (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))

-- >>> (+) 2 30
-- 32

(+++) :: List a -> List a -> List a
(+++) Nil ys         = ys
(+++) (Cons x xs) ys = Cons x ((+++) xs ys)


-- append xs ys   ========= xs +++ ys

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)


rev :: List a -> List a
rev Nil         = Nil
rev (Cons x xs) = append (rev xs) (Cons x Nil)

revT l = loop Nil l
  where
    loop res Nil         = res
    loop res (Cons x xs) = loop (Cons x res) xs

{-
rev (Cons 1 (Cons 2 (Cons 3 Nil)))

loop Nil (Cons 1 (Cons 2 (Cons 3 Nil)))
loop (Cons 1 Nil) ((Cons 2 (Cons 3 Nil)))
loop (Cons 2 (Cons 1 Nil) (((Cons 3 Nil)))
loop (Cons 3 (Cons 2 (Cons 1 Nil) (((Nil)))

-}



-- >>> size list0
-- 3

{-
< size (Cons 1 (Cons 2 (Cons 3 ... (Cons 100000000000 Nil ))) >
==>
<1 + < size ((Cons 2 (Cons 3 ... (Cons 100000000000 Nil ))) > >

<1 + < 1 + < (size (Cons 2 (Cons 3 ... (Cons 100000000000 Nil ))) > > >

<1 + < 1 + < 1 + < 1 + ... <1 + 0> > > > >

def size(l):
  res = 0
  while l != Nil:
    res += 1
    l = tail(l)
  res

-}


size :: List a -> Int
size Nil = 0
size (Cons _ t) = 1 + size t




data Tree = Leaf | Node Int Tree Tree
  deriving (Show)

depth :: Tree -> Int
depth Leaf                = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

tree0 :: Tree
tree0 = Node 1
          (Node 2
            (Node 3
              Leaf
              Leaf)
            Leaf)
          (Node 4
            Leaf
            Leaf)

-- >>> sumTo 10
-- 55

sumTo' :: Int -> Int
sumTo' 0 = 0
sumTo' n = n + sumTo' (n-1)

sumTo :: Int -> Int
sumTo m = loop m 0
  where
   loop n res
     | n <= 0    = res
     | otherwise = loop (n - 1) (res + n)



{-

def sumTo(n):
  res = 0
  while n > 0:
    res = r + n
    n   = n - 1
  res

  sumTo 5
  ==>
  <5 + sumTo 4>
  ==>
  <5 + <4 + sumTo 3>>
  ==>
  <5 + <4 + <3 + sumTo 2>>>
  ==>
  <5 + <4 + <3 + <2 + sumTo 1>>>>
  ==>
  <5 + <4 + <3 + <2 + <1 + sumTo 0>>>>>


  sumTo 5
  ==>
  loop 5 0
  ==>
  loop 4 5
  ==>
  loop 3 9
  ==>
  loop 2 12
  ==>
  loop 1 14
  ==>
  loop 0 15
  ==>
  15




-}
