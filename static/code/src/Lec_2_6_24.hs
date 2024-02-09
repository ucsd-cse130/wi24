{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lec_2_6_24 where

import Prelude hiding (length)
import Text.Printf (printf)

-- >>> clone 3 "dog"
-- ["dog","dog","dog"]

-- >>> bob 10
-- 11

clone :: Int -> a -> [a]
clone 0 _ = []
clone n x = x : clone (n-1) x


bob x = x + 1

-- >>> thdElt ["cat", "octo", "nouse"]
-- "nouse"

-- Prelude.undefined

-- >>> grab 4 [0,1,2,3,4]
-- /Users/rjhala/teaching/wi24/static/code/src/Lec_2_6_24.hs:(68,1)-(71,37): Non-exhaustive patterns in function grab

-- >>> grab 1 [0,1,2,3,4,5,6,7]
-- [0]

-- >>> grab 2 [0,1,2,3,4,5,6,7]
-- [0,1]

-- >>> grab 3 [0,1,2,3,4,5,6,7]
-- [0,1,2]







firstElt :: [a] -> a
firstElt l = case l of
              h:_ -> h
              _   -> undefined

thdElt :: [a] -> a
thdElt l = case l of
            _: ( _ : ( h3 : _ ) ) -> h3
            _ -> undefined

firstElem :: [a] -> a
firstElem (x1:_) = x1
firstElem _      = undefined



secondElem :: [a] -> a
secondElem (_:x2:_) = x2
secondElem _        = undefined

thirdElem :: [a] -> a
thirdElem (_x1 : (_x2 : (x3 : _x4))) = x3
thirdElem _          = undefined

rjList = ["dog", "cat"]


grab :: Int -> [stuff] -> [stuff]
grab 0 _               = []
grab _ []              = []
grab n (x:xs)          = x : (grab (n-1) xs)

-- 4 + (9 * 10)

-- grab 1 (x0:_)          = [x0]
-- grab 2 (x0:x1:_)       = [x0, x1]
-- grab 3 (x0:x1:x2:x3:_) = [x0, x1, x2]

-- >>> grab 1 [0,1,2,3,4,5,6,7]
-- [0]

-- >>> grab 2 [0,1,2,3,4,5,6,7]
-- [0,1]

-- >>> grab 3 [0,1,2,3,4,5,6,7]
-- [0,1,2]


-- mys l = case l of
--           [] -> 0
--           x:xs -> 1 + mys xs

length [] = 0
length (x:xs) = 1 + length xs


-- sumList (x:xs)     = x + sumList xs
sumList []         = 0
sumList (x1:xs) = x1 + (sumList xs)



{-
mys (10:(20:(30:[])))
==> 1 + mys (20:30:[])
==> 1 + (1 + mys (30:[]))
==> 1 + (1 + 1 + mys [])
==> 1 + (1 + 1 + 0)
==> 3
-}


data Date = MkDate
  { month :: Int    -- generates `day :: Date -> Int`
  , day   :: Int
  , year  :: Int
  }

-- data Bob = MkBob {
--   day :: Int        -- day :: Bob -> Int
--   }



today :: Date
today = MkDate 2 6 2024

-- >>> getYear today
-- 2024

getYear :: Date -> Int
getYear (MkDate _ _ y) = y

getHour :: Time -> Int
getHour (MkTime h _ _) = h

date24 :: Int -> Int -> Date
date24 month day = MkDate month day 2024

date_2_6 :: Int -> Date
date_2_6 =  MkDate 2 6

-- \x -> f x  ==== f


data Time = MkTime Int Int Int

now :: Time
now = MkTime 13 5 55

data Student = MkStudent
  { name :: String
  , graduation :: Date
  }




-- quiz = nextDate now
-- >>> nextDate now
-- Couldn't match expected type `Date' with actual type `Time'
-- In the first argument of `nextDate', namely `now'
-- In the expression: nextDate now
-- In an equation for `it_a73ke': it_a73ke = nextDate now

-- (A) "undefined"
-- (B) type mismatch
-- (C) ???


nextDate :: Date -> Date
nextDate = undefined


data Doc
  = MkHeading Int String
  | MkPlainText String
  | MkList Bool [String]

data Blah
  = Int
  | Bool

blook :: [Blah]
blook = [Int, Int, Bool, Bool]

quiz99 :: Doc
quiz99 = MkPlainText "hey there!"
{-

What is the type of `quiz`?

(A) String
(B) Doc
(C) Type Error
(D) Other

-}
doc :: [Doc]
doc = [ MkHeading 1 "Notes from 130"                  -- Level 1 heading
      , MkPlainText "There are two types of languages:"     -- Plain text
      , MkList True [ "those people complain about"  -- Ordered list
                    , "those no one uses"]
      ]

docHtml :: Doc -> String
docHtml d = case d of
              MkHeading lvl s -> printf "<h%d>%s</h%d>" lvl s lvl
              MkPlainText str -> printf "<p>%s</p>" str
              MkList ord items -> undefined
