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
module Lec_2_20_24 where

import Prelude hiding (map, filter, foldr, foldl)
import Data.Char (toUpper, ord)
import Data.List (intercalate)

-- >>> cat ["carne", "asada"]
-- "carneasada"

cat :: [String] -> String
cat = foldr (++) ""
-- cat []     = ""
-- cat (x:xs) = x ++ cat xs

total :: [Int] -> Int
total = foldr (+) 0
-- total []     = 0
-- total (x:xs) = x + total xs

foldr op b []     = b
foldr op b (x:xs) = op x (foldr op b xs)
{-
foo []     = ""
foo (x:xs) = x ++ foo xs


foo []     = 0
foo (x:xs) = x + foo xs

foo op b []     = b
foo op b (x:xs) = op x (foo xs)

-}






len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs







-- >>> ord 'a'
-- 97

-- >>> ords ['a' .. 'z']
-- [97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122]

shout :: [Char] -> [Char]
shout = map toUpper
-- shout []     = []
-- shout (x:xs) = toUpper x  : shout xs

-- >>> quiz
-- [3,5]

quiz :: [Int]
quiz = map (\(x, y) -> x + y) [(1,2), (2,3)]

powersOfTwo :: [Int] -> [Int]
powersOfTwo = map (\x -> 2 ^ x)
-- powersOfTwo []     = []
-- powersOfTwo (x:xs) = (2 ^ x) : powersOfTwo xs

ords :: [Char] -> [Int]
ords = map ord
-- ords [] = []
-- ords (x:xs) = ord x : ords xs

map :: (a -> b) -> [a] -> [b]
map _  []     = []
map f (x:xs) = f x : map f xs


{-
foo []     = []
foo (x:xs) = toUpper x  : foo xs
foo toUpper

foo []     = []
foo (x:xs) = (2 ^ x) : foo xs

foo []     = []
foo (x:xs) = ord x : foo xs

foo op []     = []
foo op (x:xs) = op x : foo op xs

-}



-- >>> squares [1,2,3,4,5]
-- [1,4,8,16,32]



fac :: Int -> Int
fac n
  | n <= 1    = 1
  | otherwise = n * fac (n-1)


-- >>> facTR 5
-- 120

facTR :: Int -> Int
facTR n = loop 1 n
  where
    loop acc k
      | k <= 1    = acc
      | otherwise = loop (acc * k) (k - 1)
{-
  fac 4
  ==>
  loop 1 4
  ==>
  loop (1*4) 3
  ==>
  loop (1*4*3) 2
  ==>
  loop (1*4*3*2) 1
  ==>
  24

  < fac 4 >
  ==>
  4 * < fac 3 >
  ==>
  4 * < 3 * < fac 2 > >
  ==>
  4 * < 3 * < 2 * <fac 1> > >
  ==>
  4 * < 3 * < 3 * <1> > >

-}
-- >>> mod 101 2
-- 1

-- >>> evens' [1,2]
-- [2]

evens :: [Int] -> [Int]
evens []      = []
evens (x:xs)  = let rest = evens xs
                in if even x then x : rest else rest

evens' :: [Int] -> [Int]
evens' = filter even

{-
  evens' [1,2]
 ==>
  filter even [1,2]
 ==>
  if even 1 then 1 : (filter even [2]) else (filter even [2])
 ==>
  filter even (2:[])
 ==>
  if even 2 then 2 : (filter even []) else (filter even [])
 ==>
  2 : (filter even [])
 ==>
  2 : ([])
 =>
  [2]
-}

-- >>> fourLetterWords ["you", "must", "eat", "a", "taco"]
-- ["must", "taco"]

fourLetterWords :: [String] -> [String]
fourLetterWords []     = []
fourLetterWords (x:xs) = let rest = fourLetterWords xs
                         in if length x == 4 then x : rest else rest

fourLetterWords' :: [String] -> [String]
fourLetterWords' = filter fourLetters

-- foo x == bar x
fourLetters :: String -> Bool
fourLetters x = length x == 4

filter :: (a -> Bool) -> [a] -> [a]
filter _    []     = []
filter cond (x:xs) = let rest = filter cond xs
                     in if cond x then x : rest else rest
{-

foo []      = []
foo (x:xs)  = let rest = foo xs
              in if even x       then x : rest else rest



foo []     = []
foo (x:xs) = let rest = foo xs
             in if fourLetters x then x : rest else rest






-}


----

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show)

class Tot t where
  adder :: Integer -> t

instance Tot Integer where
  adder x = x

instance (Integral a, Tot r) => Tot (a -> r) where
  adder acc x = adder (acc + toInteger x)

tot :: (Tot a) => a
tot = adder 0

-- >>> [tot 0, tot 0 1, tot 0 1 2 3 4 5 6 7 9 10]

class Printer t where
  printer :: [String] -> t

instance Printer String  where
  printer = intercalate ", "

instance (Show a, Printer r) => Printer (a -> r) where
    printer acc x = printer (acc ++ [show x])

funny :: (Printer t) => t
funny = printer []

-- >>> bob
-- ["1","1, 2","1, 2, \"cat\"","1, 2.4, [\"dog\",\"hours\"]","Sat, 1, 2.4, [Sun,Mon,Tue]"]

bob :: [String]
bob = [funny 1,
       funny 1 2,
       funny 1 2 "cat",
       funny 1 2.4 ["dog", "hours"],
       funny Sat 1 2.4 [Sun, Mon, Tue]
       ]



-- main :: IO ()
-- main = do printAll 5 "Mary" "had" "a" "little" "lamb" 4.2 -- note: the arguments can even be different types
--           printAll 4 3 5
