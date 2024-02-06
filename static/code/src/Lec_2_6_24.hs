{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
module Lec_2_6_24 where

ex0 :: Int
ex0 = 10 + 10 + 1

ex1 :: Double
ex1 = 10.0 + 10.0 + 1.0

ex2 :: Char
ex2 = 'c'

ex3 :: Bool
ex3 = True

ex4 :: Bool
ex4 = False

-- >>> isPos (12 - 100)
-- False

-- >>> isPos 1000
-- True


-- isPos = \n -> n > 0
isPos :: Int -> Bool
isPos n = n > 0

ex10 :: Bool
ex10 = isPos 10

-- >>> (((pat 10) 20) 30)

pat :: Int -> Int -> Int -> Int
pat x y z  =  x * (y + z)
--
-- pat = \x y z -> x * ( y + z )
-- pat = \x -> \y -> \z -> x * ( y + z )

frog = pat 10 20

-- >>> frog 30
-- 500


{-
frog
==> pat 10 20
==> (((\x -> \y -> \z -> x * ( y + z )) 10) 20)
==> \z -> 10 * (20 + z)

frog 30
==> (\z -> 10 * (20 + z)) 30
==> 10 * (20 + 30)
==> 500

frog 30
==> (pat 10 20) 30
==> 10 * (20 + 30)



frog
==>


-}

ite :: Bool -> t -> t -> t
ite b vThen vElse = if b then vThen else vElse

ifThen :: (Num t) => Bool -> t -> t
ifThen b vThen = if b then vThen else 0

-- >>> 4 + 5.1
-- 9.1


-- >>> 5 + 6
-- 11

-- >>> 5.4 + 6.43
-- 11.83

{-





A: Yes, it typechecks
B: Noooo...
-}

-- takes two Int  and returns their maximum
myMax :: Int -> Int -> Int
myMax x y = ite (x>y) x y






-- >>> bob
-- 500

bob :: Int
bob = pat 10 20 30

{-
pat 10 20 30
==> 10 * (20 + 30)
==> 10 * 50
==> 500




-}

-- >>> dec 100
-- 9

dec :: Int -> Int
dec n = if isZ n then 0 else fst (loop n shift (pair 0 0))

shift = \p -> pair (snd p) (inc (snd p))

isZ = \n -> n == 0
pair x y = (x, y)
loop :: Int -> (a -> a) -> a -> a
loop 0 _ x = x
loop n f x = loop (n-1) f (f x)




-- add2 :: Int -> Int -> Int
--add2 x y = x + y + 10
--add2     = \x y -> x + y + 10

{-
quiz :: Int
quiz = if 5 > 10 then ex0 else ex4


ex0 :: Int
ex0 = 10 + 10 + 1

A. Int
B. Bool
C. Compiler YELLS : TYPE ERROR
-}


{-

var_1 :: type_1
var_1 = expr_1

var_2 :: type_2
var_2 = expr_2


-}


-- >>> inc 10
-- 11







inc :: Int -> Int
inc x = x + 1

{-

def sum(n):
    if n == 0:
        return 0
    else:
        return n + sum(n - 1)

let sum = \n -> ITE (ISZERO n) ZERO (ADD n (sum (DECR n))



n f x

def loop(n, f, x):
  res = x
  for i in range(0, n):
    res = f(res)
  res


def sum(n):
  loop(n, ?f, 0)

def f(res, i):
    (res + i, i + 1)

def sum(n):
  res = 0
  for i in range(1, n):
    res += i
  res



-}

-- >>> sumTo 3
-- 6

sumTo :: Int -> Int
sumTo 0 = 0
sumTo n = n + sumTo(n-1)


blah = (True, "ten", True)

-- >>> snd3 (10, 20, 30, 40)
-- "ten"

-- funkyList = "cat" : "dog" :  "orse" : []
funkyList = ["cat", "dog", "orse" ]


-- >>> copy3 10
-- No instance for (Num String) arising from the literal `10'
-- In the first argument of `copy3', namely `10'
-- In the expression: copy3 10
-- In an equation for `it_a2udS': it_a2udS = copy3 10

copy3 :: a -> [a]
copy3 c =  [c,c,c]

-- >>> clone 3 "cat"
-- ["cat","cat","cat"]

-- >>> range 4 30
-- [4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]

-- >>> "cat" : ["dog", "mouse"]
-- ["cat","dog","mouse"]

clone :: Int -> a -> [a]
clone 0 _ = []
clone n x = x : clone (n-1) x


-- range lo hi  ==> [lo, lo+1, lo+2,... hi]
range :: Int -> Int -> [Int]
-- range 4 3 =      []
-- range 3 3 =     [3]
-- range 2 3 =   [2,3]
-- range 1 3 = 1:[2,3]
range lo hi = if lo <= hi then lo : range (lo+1) hi else []

-- >>> ['c', 'a', 't'] == "cat"
-- 'a'

firstElem :: [a] -> a
-- firstElem l = case l of
--                     x1 : _ -> x1
--                     []     -> undefined

-- >>> take 10 (foo 10)
-- [10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109]

firstElem (x1:_) = x1
firstElem _      = undefined

foo :: Int -> [Int]
foo n = n : foo (n+1)

secondElem :: [a] -> a
secondElem (_:x2:_) = x2
secondElem _        = undefined

thirElem :: [a] -> a
thirElem (_x1 : (_x2 : (x3 : _x4))) = x3
thirElem _          = undefined

{-
  case ("cat" : ("dog" : [])) of
         x1   : x2 ->   x1 := "cat"
                   x2 := ("dog" : [])

  case ("cat" : ("dog" : [])) of
         x1   : (x2    : x3) ->   x1 := "cat"
                                  x2 := "dog"
                                  x3 := []


-}



{-
  firstElem ("cat", "dog")

-}






{-
  clone 3 "cat"
==> "cat" : clone 2 "cat"
==> "cat" : ( "cat" : clone 1 "cat")
==> "cat" : ( "cat" : ("cat" : clone 0 "cat") )
==> "cat" : ( "cat" : ("cat" : []) )
==> ["cat", "cat", "cat"]

-}

















-- fst4 :: (a1,a2,a3,a4) -> a1
mango :: (Int, Int, Int) -> Int
mango t = case t of
            (x1,_,_) -> x1

-- fst4 t = case t of
--              (x1,_,_,_) -> x1


fst3 :: (t1, t2, t3) -> t1
fst3 t = case t of
            (x1,_,_) -> x1

-- fst3 (x1,_,_) = x1

snd3 :: (t1, t2, t3) -> t2
snd3 t = case t of
           (_,x2,_) -> x2


{-
bloh :: (Int-> Int, Int-> Int, Int -> Int -> Int)
bloh = (\x -> x + 1,        -- :: Int -> Int
        \y -> y - 1,        -- :: Int -> Int
        myMax               -- :: Int -> Int -> Int
        )


-}

{-
sumTo 3
=> 3 + sumTo (3-1)
=> 3 + sumTo 2
=> 3 + (2 + sumTo (2-1))
=> 3 + (2 + sumTo 1)
=> 3 + (2 + (1 + sumTo (1-1)))
=> 3 + (2 + (1 + sumTo 0))
=> 3 + (2 + (1 + 0))
=> 6

-}
-- sumTo n = if n <= 0 then 0 else n + sumTo (n-1)

-- 55
