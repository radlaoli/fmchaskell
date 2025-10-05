{-# LANGUAGE GADTs #-}

module FMCList where

import FMCNat
import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a 
head [] = error "empty list"
head (x:xs) = x 

tail :: [a] -> [a]
tail []= error "empty list"
tail (x:xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False 

length :: Integral i => [a] -> i
length [] = 0
length (x:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs 

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]


(++) :: [a] -> [a] -> [a]
(++) [] xs = xs
(++) (x:xs) ys = x : (xs++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs= xs ++ [x] 

--flip snoc
(<:) :: [a] -> a -> [a]
(<:) xs x = snoc x xs

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: (Ord a) => [a] -> a
minimum [] = error "empty list"
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

take :: Nat -> [a] -> [a]
take O _ = []
take _ [] = []
take n (x:xs) = x: take (n - 1) xs

drop :: Nat -> [a] -> [a]
drop O xs  = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs) =
  case f x of
  True -> x: takeWhile f xs
  False -> [] 

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x:xs) =
  case f x of
  False -> x:xs
  True -> dropWhile f xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs

init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)


any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) = f x || any f xs

all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = f x && all f xs

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = True
or (x:xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xs') = xs ++ concat xs'

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem y xs = any (== y) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = (y == x) || elem y xs

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x 
(x:xs) !! n = xs !! (n-1)        

filter :: (a -> Bool) -> [a] ->  [a]
filter _ [] = []
filter p (x:xs) 
  | p x       = x : filter p xs
  | otherwise = filter p xs

map :: (a -> b) -> [a] -> [b]
map p [] = []
map p (x:xs) = p x : map p xs

cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs


repeat :: a -> [a]
repeat n = n : repeat n


replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n m = m : replicate (n-1) m 

isPrefixOf :: Eq a => [a] -> [a] -> Bool 
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs ys = isPrefixOf xs ys || isInfixOf xs (tail ys)

isSuffixOf :: Eq a => [a] -> [a] -> Bool 
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf xs ys = reverse xs `isPrefixOf` reverse ys

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y): zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f _ [] = []
zipWith f [] _ = []
zipWith f (x:xs) (y:ys) = (f x y): zipWith f xs ys


-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
