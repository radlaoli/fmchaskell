{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n 

instance Eq Nat where
    (==) O O = True
    (==) (S n) (S m) = n == m
    (==) _ _ = False

instance Ord Nat where

    (<=) O O = True
    (<=) _ O = False
    (<=) O (S _) = True
    (<=) (S n) (S m) = n <= m 

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = O
    min _ O = O
    min (S n) (S m) = 
        case n <= m of
        True -> S n
        False -> S m  

    max O n = n
    max n O = n
    max (S n) (S m) =
        case n <= m of
        True -> S m
        False -> S n


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S n) = odd n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n 

----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven


----------------------------------------------------------------
-- operations
----------------------------------------------------------------
-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) O n = n
(<+>) (S n) m = S (n <+> m)
infixl 6 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.

monus :: Nat -> Nat -> Nat
monus O n = O
monus n O = n
monus (S n) (S m) = monus n m

(<->) :: Nat -> Nat -> Nat
(<->) = monus
infixl 6 <->

-- multiplication
times :: Nat -> Nat -> Nat
times O _ = O
times (S n) m = m <+> (times n m)

(<*>) :: Nat -> Nat -> Nat
(<*>) = times
infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow n O = S O
pow O (S m) = O
pow n (S m) = n <*> pow n m

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow
infixr 8 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) n O = undefined
(</>) n (S m) =
  case n <-> m of
    O -> O 
    _ -> S ((n <-> S m) </> S m)
infixl 7 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ O = undefined
(<%>) n m = n <-> m <*> (n </> m) 
infixl 7 <%>

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (O , _)= undefined 
eucdiv (_ , O)= (O,O)
eucdiv (n , m)= (n</>m,n<%>m)

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) O _ = undefined
(<|>) _ O = True
(<|>) m n = isZero (n <%> m)

divides = (<|>)
infix 4 <|>

-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n m = 
  case (n <-> m) of
  O -> m <-> n
  _ -> n <-> m

(|-|) = dist
infixl 6 |-|

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = (S n) <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = undefined
lo (S O) _ = undefined 
lo b a =
  case a </> b of
    O -> O
    q -> S (lo b q)


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat n = S (toNat (n - 1)) 

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.

instance Num Nat where


    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = O
      | x == 0    = O
      | otherwise = S(fromInteger (x - 1))

