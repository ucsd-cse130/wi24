module Lec_1_25_24 where


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


-- isPos = \n -> n > 0
isPos :: Int -> Bool
isPos n = n > 0

ex10 :: Bool
ex10 = isPos 10

-- >>> pat 10 20 30
-- 500

pat :: Int -> Int -> Int -> Int
pat x y z = x * (y + z)

-- >>> dec 10
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
