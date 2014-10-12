{-
Name: Ronnie Howell 
Collaborators: <"none">
Notes: doubleEveryOther was a pain in the ass
-}

module HW01 where         -- We'll learn more about this later
import Data.List

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.
-- Exercise 1
lastDigit :: Integer -> Integer
lastDigit x = mod x 10


dropLastDigit :: Integer -> Integer
dropLastDigit x = div x 10

-- Exercise 2
toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
           | x >  0 = toDigitsInner (dropLastDigit x) [lastDigit x]

toDigitsInner :: Integer -> [Integer] -> [Integer]
toDigitsInner 0 xs = xs
toDigitsInner x xs = 
  toDigitsInner (dropLastDigit x) (lastDigit x : xs)

-- Exercise 3
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []                                                      -- Empty
doubleEveryOther (x:[]) = [x]                                                 -- One Element
doubleEveryOther (x:y:xs) = (x * 2) : y : doubleEveryOther(xs)

-- Exercise 4
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (expandDigits xs)

expandDigits :: [Integer] -> [Integer]
expandDigits [] = []
expandDigits (x:xs) = (toDigits x) ++ (expandDigits xs)

-- Exercise 5
validate :: Integer -> Bool
validate x = (sumDigits (expandDigits (doubleEveryOther (toDigits x)))) `mod` 10 == 0 

-- Exercise 6
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi(n-1) c b a

