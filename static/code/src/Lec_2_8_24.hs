{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Lec_2_8_24 where

import Prelude
import Text.Printf (printf)

data Paragraph              -- ^ THREE constructors, w/ different parameters
  = PText    String         -- ^ text: plain string
  | PHeading Int   String   -- ^ head: level and text (Int & String)
  | PList    Bool [String]  -- ^ list: ordered? & items (Bool & [String])

paragraphs :: [Paragraph]
paragraphs = [ para1 , para2 , para3 ]
  where
    para1 :: Paragraph
    para1 = PHeading 2 "Plan for this week"
    para2 :: Paragraph
    para2 = PText "Last Week"

para3 :: Paragraph
para3 = PList True [ "writing", "functions", "data", "types"]


-- >>> html para3
-- "<ol><item>writing</item><item>functions</item><item>data</item><item>types</item></ol>"

html :: Paragraph -> String
-- html (PText s)        = printf "<p>%s</p>" s
-- html (PHeading n s)   = printf "<h%d>%s</h%d>" n s n
-- html (PList False is) = printf "<ul>%s</ul>" (items is)
-- html (PList True is)  = printf "<ol>%s</ol>" (items is)
html p = case p of
          (PText s)        -> printf "<p>%s</p>" s
          (PHeading n s)   -> printf "<h%d>%s</h%d>" n s n
          (PList False is) -> printf "<ul>%s</ul>" (items is)
          (PList True is)  -> printf "<ol>%s</ol>" (items is)

-- >>> quiz67
-- "Hey There!"

blah = PText "Hey There!"

data Funky = FInt Int | FBool Bool | FStr String

-- quiz67 :: String
quiz67 = case blah of
            PText str      -> str
            PHeading _ txt -> txt
            PList ord _    -> "list"

-- data Bool = True | False
-- if e1 then e2 else e3        case e1 of True -> e2 | False -> e3


-- html (PList False [i1, i2]) = printf "<ul>%s%s</ul>" (item i1) (item i2)
-- html (PList True [i1, i2])  = printf "<ol>%s%s</ol>" (item i1) (item i2)


items :: [String] -> String
items is = case is of
            []       -> ""
            (i:rest) -> item i ++ items rest

foo :: [[Char]]
foo =  ('a': 'b': 'c': []) : []

boo = [1, 2, undefined, 4, undefined]

-- >>> (head (tail (tail boo)))
-- Prelude.undefined


{-

type String = [Char]
t

Q: Could we use `:` instead of `++` in the definition of `items` ?

(A) YES
(B) nope.

-}


item :: String -> String
item s = printf "<item>%s</item>" s



{-
A: syntax error
B: type error
C: Paragraph
D: [Paragraph]
E: [String]
-}


data Nat = Zero | Succ Nat deriving (Show)

toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + (toInt n)

decr :: Nat -> Nat
decr Zero = Zero
decr (Succ n) = n

one = Succ Zero

{-
dec (Succ Zero) == Zero
dec (Succ (Succ Zero)) = (Succ Zero)
dec (Succ (Succ (Succ Zero))) == (Succ (Succ Zero))


toInt (Succ (Succ (Succ Zero)))
==> 1 + (toInt (Succ (Succ Zero)))
==> 1 + (1 + toInt (Succ Zero))
==> 1 + (1 + (1 + toInt Zero))
==> 1 + (1 + (1 + 0))
==> 3

-}

-- toInt n = case n of
--             Zero -> 0
--             Succ m -> ???