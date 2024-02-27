{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Use sum" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Eta reduce" #-}
module Lec_2_27_24 where

import Prelude hiding (map, filter, foldr, foldl)
import Data.Char (toUpper, ord)
import Data.List (intercalate)


type Id = String

data Op = Add | Sub | Mul | Div deriving (Show)
data Expr
  = ENum Int
  | EBin Op Expr Expr
  | EVar Id
  deriving (Show)

data Value
  = VNull
  | VInt Int
  deriving (Show)

type Env = [(Id, Value)]

vNull :: Value
vNull = VNull

v105 :: Value
v105 = VInt 105

-- 10 + 20
exp0 :: Expr
exp0 = (EBin Add (ENum 10) (ENum 20))

-- x + 20
exp1 :: Expr
exp1 =  (EBin Add (EVar "x") (ENum 20))

-- >>> eval env0 (EVar "x")
-- VInt 10

-- >>> eval env0 (EVar "y")
-- VInt 20

-- >>> eval env0 (EVar "mickeymouse")
-- VNull

-- [x := 10, y := 20]
env0 :: Env
env0 = [ ("x", VInt 10), ("y", VInt 20) ]

-- >>> eval env0 exp1
-- VInt 30
eval :: Env -> Expr -> Value
eval _   (ENum n)        = VInt n
eval env (EBin op e1 e2) = evalOp op (eval env e1) (eval env e2)
eval env (EVar x)        = lookupEnv x env

lookupEnv :: Id -> Env -> Value
lookupEnv x ((key, val) : rest) = if x == key then val else lookupEnv x rest
lookupEnv x []                  = VNull



evalOp :: Op -> Value -> Value -> Value
evalOp Add (VInt n1) (VInt n2) = VInt (n1 + n2)
evalOp Mul (VInt n1) (VInt n2) = VInt (n1 * n2)
evalOp Sub (VInt n1) (VInt n2) = VInt (n1 - n2)
evalOp Div (VInt n1) (VInt n2) = if n2 == 0 then VNull else VInt (n1 `div` n2)
evalOp _    _        _         = VNull

{-
evalOp :: Op -> Value -> Value ->

-}











-- >>> cat ["carne", "asada"]
-- "carneasada"

{-

  foo x = bar x

  foo = bar
-}

cat :: [String] -> String
cat xs = foldr (++) "" xs
-- cat []     = ""
-- cat (x:xs) = x ++ cat xs



-- >>> total [1,2,3,4,5]
-- 15

-- >>> len' [1,2,3,3,4,5]
-- 6

-- >>> totalTR [1,2,3,3,4,5]
-- 18

-- >>> concatTR ["cat", "dog", "mouse"]
-- "catdogmouse"

-- >>> rev [1,2,3,4,5,6]
-- [6,5,4,3,2,1]


total :: [Int] -> Int
total xs = foldr (+) 0 xs
-- total []     = 0
-- total (x:xs) = x + total xs

totalTR :: [Int] -> Int
totalTR ys = helper 0 ys
  where
   helper acc []     = acc
   helper acc (x:xs) = helper (acc + x) xs

totalTR' :: [Int] -> Int
totalTR' = foldl (+) 0

concatTR :: [String] -> String
concatTR ys = helper "" ys
  where
   helper acc []     = acc
   helper acc (x:xs) = helper (acc ++ x) xs

concatTR' :: [String] -> String
concatTR' ys = foldl (++) "" ys

foldl op acc []     = acc
foldl op acc (x:xs) = foldl op (op acc x) xs


rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

{-

op = \acc x -> x : acc
acc = []




foldl op acc (x1:x2:x3:[])
-->
foldl op (op acc x1) (x2:x3:[])
-->
foldl op (op (op acc x1) x2) (x3:[])
-->
foldl op (op (op (op acc x1) x2) x3) []
-->
(((acc `op` x1) `op` x2) `op` x3)

(x3:(x2:(x1 : [] ) ))

==
x3 : (x2 : (x1 : []))

acc === []

op acc x = x:acc



-}



{-
   helper acc []     = acc
   helper acc (x:xs) = helper (acc + x) xs


   helper acc []     = acc
   helper acc (x:xs) = helper (acc ++ x) xs



-}








foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op b []     = b
foldr op b (x:xs) = op x (foldr op b xs)

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs


len' :: [a] -> Int
len' = foldr (\_ n -> 1 + n) 0

{-


  (A) Yes `len'` is equivalent to `len`
  (B) Nope it produces a different result
  (C) ???


-}

{-
  <foldr op b  (x1:x2:x3:[])>
  ==>
  <op x1 <foldr op b (x2:x3:[]))>>
  ==>
  <op x1 <op x2 <foldr op b (x3:[])>>>
  ==>
  op x1 (op x2 (op x3 (foldr op b [])))
  ==>
  op x1 (op x2 (op x3 b))

  x1 `op` (x2 `op` (x3 `op` b))

  x1 :     x2  :    x3  :  []









-}




{-
foo = foldr (++) ""
foo []     = ""
foo (x:xs) = x ++ foo xs

foo = foldr (+) 0
foo []     = 0
foo (x:xs) = x + foo xs

-}
