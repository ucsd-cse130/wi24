{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# LANGUAGE DeriveFunctor #-}
module Lec_3_14_24 where

-- >>> foo "10" "20"
-- True
data Option a = Null | Some a
    deriving (Show)

-- instance Show a => Show (Option a) where
--     -- show :: Option a -> String
--     show Null = "Null"
--     show (Some v) = "Some(" ++ show v ++ ")"

instance Eq a => Eq (Option a) where
    (==) Null Null          = True
    (==) Null (Some _)      = False
    (==) (Some _) Null      = False
    (==) (Some x) (Some y)  = x == y


-- From STDLIB Eq Int

-- From def above we get: Eq (Option Int)

-- From def above we get: Eq (Option (Option Int))

-- >>> Some (Some(1+1)) == Some(Some 2)
-- True

-- >>> get "cat" [("apple", 10), ("cat", 20), ("dog", 30)]
-- Some(20)

-- >>> get "dog" [("apple", 10), ("cat", 20), ("dog", 30)]
-- Some(30)

-- >>> get "chihuahua" [("apple", 10), ("cat", 20), ("dog", 30)]
-- Null

-- >>> (read "(10.3, 5)") :: (Double , Int)
-- (10.3,5)

get :: Eq t => t -> [(t, a)] -> Option a
get _   [] = Null
get key ((k,v) : rest) = if key == k then Some v else get key rest



foo :: Ord a => a -> a -> Bool
foo x y = x < y

inc :: Num a => a -> a
inc x = x + 1

incD :: Double -> Double
incD x = x + 1

{-

class Num t where
    (+) :: t -> t -> t

instance Num Int where
    (+) x y = integer-add-x-y


trait Num<T> {
    fn plus(x:T, y:T) -> T

}

interface Num<T> {
    T plus(T x, T y)
}

funky x = x + True
What is the type of (+) ?

(a) Int -> Int -> Int
(b) Double -> Double -> Double
(c) a -> a -> a
(d) has no type
(e) ???

-}



data Tree a = Node (Tree a) (Tree a) | Leaf a
    deriving (Show, Functor)

tree0 :: Tree Int
tree0 = Node (Node
                (Leaf 1)
                (Leaf 2)
             )
             (Node
                (Leaf 3)
                (Leaf 4)
             )

-- >>> fmap (^2) tree0

-- >>> nap (^2) (Some 92)
-- Some 8464

-- >>> :i Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor t where
--   fmap :: (a -> b) -> t a -> t b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
--   	-- Defined in ‘GHC.Base’
-- instance Functor (Either a) -- Defined in ‘Data.Either’
-- instance Functor [] -- Defined in ‘GHC.Base’
-- instance Functor Solo -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance Functor IO -- Defined in ‘GHC.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Base’
-- instance Functor ((,,,) a b c) -- Defined in ‘GHC.Base’
-- instance Functor ((,,) a b) -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’



class Nappable t where
  nap :: (a -> b) -> t a -> t b

instance Nappable Option where
    nap f Null     = Null
    nap f (Some v) = Some (f v)

instance Nappable Tree where
   nap f (Node l r) = Node (nap f l) (nap f r)
   nap f (Leaf x)   = Leaf (f x)

instance Nappable [] where
  nap f [] = []
  nap f (x:xs) = f x : nap f xs

{-


mapList :: (a -> b) -> List a -> List b
mapTree :: (a -> b) -> Tree a -> Tree b

-}


data Expr
    = Number Int
    | Add Expr Expr
    | Div Expr Expr
    deriving (Eq, Ord, Show)

-- >>> append (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons 4 (Cons 5 Nil))
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))

-- >>> eval ((Add (Number 10) (Number (-10))))
-- Val 0

-- >>> eval expr0
-- Err (Add (Number 10) (Number (-10)))

expr0 :: Expr
expr0 = Div (Number 100) (Add (Number 10) (Number (-10)))

-- >>> eval expr0
-- Err (Add (Number 10) (Number (-10)))
-- >>> eval (Add (Number 10) (Number 5))
-- Val 15
data Result e v
  = Err e
  | Val v
  deriving (Show)

data List a = Nil | Cons a (List a) deriving (Show)

(+++) :: List a -> List a -> List a
(+++) Nil ys = ys
(+++) (Cons x xs) ys = Cons x (xs +++ ys)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where


instance Monad List where
    -- (>>=) :: List a -> (a -> List b) -> List b
    (>>=) Nil _         = Nil
    (>>=) (Cons x xs) f = f x +++ (xs >>= f)
    return x = Cons x Nil


thursday :: Monad m => m a -> m b -> m (a, b)
thursday thing1 thing2 = do
    x1 <- thing1
    x2 <- thing2
    return (x1, x2)


funky f xs = do
    x <- xs
    return (f x)

filt f xs = do
    x <- xs
    if f x then return x else []

-- >>> filt (>= 3) [1,2,3,4]


-- >>> funky (+100) [1,2,3,4]
-- [101,102,103,104]





thursday' thing1 thing2 =
    thing1 >>= (\x1 ->
      thing2 >>= (\x2 ->
        return (x1, x2)
      )
    )

-- >>> th
{-

[1, 2] >>= (\x1 ->
  ["cat", "dog"] >>= (\x2 ->
        return (x1, x2)
      )
    )

-}



-- >>> thursday [1,2,3] ["cat", "dog"]
-- [(1,"cat"),(1,"dog"),(2,"cat"),(2,"dog"),(3,"cat"),(3,"dog")]

-- >>> thursday (Val 10) (Val 20)
-- Val (10,20)

{-

(A) Err ()
(B) (10, 20)
(C) (Val 10, Val 20)
(D) Val (10, 20)
(E) Type Error

-}








{-

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

-}

instance Functor (Result e) where
instance Applicative (Result e) where

instance Monad (Result e) where
    (>>=) = pat
    return = ret

pat :: Result e t -> (t -> Result e v) -> Result e v
pat thing doStuff =
    case thing of
        Err e -> Err e
        Val v -> doStuff v

ret :: a -> Result e a
ret = Val

{-
    expr >>= \x ->
        anotherExpr

    do { x <- expr;
         anotherExpr
        }
-}


eval :: Expr -> Result Expr Int
eval (Number n)  = return n
eval (Add e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)

eval (Div e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      if v2 == 0 then Err e2 else return (v1 `div` v2)
