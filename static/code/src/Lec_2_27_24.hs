{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Use sum" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Eta reduce" #-}
module Lec_2_27_24 where

import Prelude hiding (map, filter, foldr, foldl)

type Id = String

data Op = Add | Sub | Mul | Div deriving (Show)
data Expr
  = ENum Int
  | EBin Op   Expr Expr
  | EVar Id
  | ELet Id   Expr Expr
  | ELam Id   Expr
  | EApp Expr Expr
  deriving (Show)

-- >>> eval [] incTest
-- VInt 11

-- >>> eval [] incTestC
-- VInt 11

-- >>> eval [] incTestCandC
-- VInt 11

-- >>> eval [] incTestCandCandC
-- VInt 111


{-
  let incr = \x -> x + 1
  in
    -- ENV = [("incr",VFun "x" (EBin Add (EVar "x") (ENum 1)))],
    -- EApp (EVar "incr") (ENum 10))
    incr 10

-}

-- >>> eval [] expAdd
-- VInt 1130

{-
                            []
let add = \x -> (\y -> x + y)
                            [add := <"x", (\y -> x + y), []> ] == env0
in
  let add10 = add 10        [add10 := <"y", x+y, (x := 10: env0) > ]
  in
    let add20 = add 20
                            [add20 := <"y", x+y, [x := 20, add10 := <"y", x+y, [x := 10]] > ]
    in
      (add10 100) + (add20 1000)
-}

expAdd :: Expr
expAdd =
  ELet "add" (ELam "x" (ELam "y" (EBin Add (EVar "x") (EVar "y"))))
    (ELet "add10" (EApp (EVar "add") (ENum 10))
      (ELet "add20" (EApp (EVar "add") (ENum 20))
        (EBin Add (EApp (EVar "add10") (ENum 100)) (EApp (EVar "add20") (ENum 1000)))))



incTest :: Expr
incTest =
  ELet "incr" (ELam "x" (EBin Add (EVar "x") (ENum 1)))
    (EApp (EVar "incr" ) (ENum 10 ))

incTestC :: Expr
incTestC =
  ELet "c" (ENum 1)
    (ELet "incr" (ELam "x" (EBin Add (EVar "x") (EVar "c")))
      (EApp (EVar "incr" ) (ENum 10 )))

incTestCandC :: Expr
incTestCandC =
  ELet "c" (ENum 1)
    (ELet "incr" (ELam "x" (EBin Add (EVar "x") (EVar "c")))
      (ELet "c" (ENum 100)
        (EApp (EVar "incr" ) (ENum 10 ))))


incTestCandCandC :: Expr
incTestCandCandC =
  ELet "c" (ENum 1)
    (ELet "incr" (ELam "x" (EBin Add (EVar "x") (EVar "c")))
      (ELet "c" (ENum 100)
        (EBin
            Add
            (EApp (EVar "incr" ) (ENum 10 ))
            (EVar "c")
        )
      )
    )





data Value
  = VNull
  | VInt  Int
  | VFun  Id Expr Env
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

env1 :: Env
env1 = [ ("z", VInt 92), ("x", VInt 9), ("y", VInt 20) ]

            -- (x + z) * y
-- >>> eval env1 (EBin Mul (EBin Add (EVar "x") (EVar "z")) (EVar "y"))
-- VInt 2020


-- >>> evalOp Add VNull undefined
-- VNull

expQuiz :: Expr
expQuiz =                               -- []
  ELet "x" (ENum 10)
                                        -- [x := 10]
    (EBin Add
                                        -- [x := 10]
      (ELet "y" (EBin Mul (EVar "x") (EVar "x"))
                                        --      [y := 100, x := 10]
        (EBin Mul (EVar "x") (EVar "y"))
      )
                                        -- [x := 10]
      (EVar "x")
    )

-- >>> eval [] expQuiz
-- VInt 1010

expr10 :: Expr
expr10 = ELet "xanadu" (ENum 999)
         (ELet "x" (ENum 1)
          (EBin Add
            (EVar "x")
            (EVar "xanadu")))

exp11 = let xanadu = 99
        in
          let x = 1
          in
            x + xanadu

-- >>> eval [] (EApp (ENum 10) (ENum 0))

-- >>> eval [("xanadu", VInt 10000)] expr10
--  VInt 10001

-- >>> blah
-- /Users/rjhala/teaching/wi24/static/code/src/Lec_2_27_24.hs:131:12-27: Non-exhaustive patterns in x1 : x2 : x3 : _

blah :: Integer
blah = let x1:x2:x3:_ = [1]
       in
          x2
-- VInt 30
eval :: Env -> Expr -> Value
eval _   (ENum n)        = VInt n
eval env (EBin op e1 e2) = evalOp op v1 v2
  where
    v1 = eval env e1
    v2 = eval env e2
eval env (EVar x)        = lookupEnv x env
eval env (ELet x e1 e2)  = eval env' e2
  where
    env' = (x, v1) : env
    v1   = eval env e1
eval env (ELam x e) = VFun x e env

eval env (EApp e1 e2) = case eval env e1 of
  VFun x body frozEnv -> eval ((x, eval env e2) : frozEnv) body
  _           -> VNull

--   error ("wtf: " ++ show (env, e))






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

let x = 999
 in           -- [x=999]
  let x = 0
  in          -- (x := 0) : x := 999 : []
              -- x := 999 : (x:=0) : []
    x + 1



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
