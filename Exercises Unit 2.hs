-- © All wrogns reserved.

import Test.QuickCheck
import Data.List


{ - 1. Define a function allDifferent :: Eq a => [a] -> Bool to test whether all elements in a 
list are different. For instance:
allDifferent [1,7,3] => True
allDifferent [1,7,3,1] => False -}

allDifferent :: Eq a => [a] -> Bool
allDifferent [] = True
allDifferent [_] = True
allDifferent (x:xs) = not(x `elem` xs) && allDifferent xs



{ - 2. Predefined function replicate :: Int -> a -> [a] takes a natural number n and a value x
and returns a list with n repetitions of x.
a) Define your own version of this function (call it replicate') by using list comprenhensions:
  replicate' 3 0 => [0,0,0]
  replicate' 4 'a' => "aaaa" -}
  
replicate' :: Int -> a -> [a]
replicate' n x = [x | b <- [1..n]]

{ - b) Read and understand the following property on replicate':
  p_replicate' n x = n >= 0 && n <=1000 ==> length (filter (==x) xs) == n && length (filter (/=x) xs) == 0
      where
          xs = replicate' n x
c) Test this property usgin QuickCheck. -}



{ - 3. Define using list comprenhensions a function called divisors returning all naturals divisors of a
natural number:
  divisors 10 => [1,2,5,10]
Define another function returning positive and negative divisors for an integer number:
  divisors (-10) => [-10,-5,-2,-1,1,2,5,10] -}

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

divisors' :: Int -> [Int]
divisors' n = [x | x <- [(-a)..a], x /= 0 && n `mod` x == 0]
    where
        a = abs n



{ - 4. Greatest common divisor of two numbers x and y, denoted by gcdiv x y, is the maximum from the
set comprising common divisors for x and y. Recall that greatest common divisor of two numbers is
only defined if x and y are not simultaneously zero.
a) Define, using a list comprenhension, a function gcdiv returning the greatest common divisor of
two numbers. For instance:
  gcdiv 30 75 => 15
You will have to return the maximum element of the list that includes common divisors for both
numbers using the following predefined function:
  maximun ::  (Ord a) => [a] -> a -}

gcdiv :: Int -> Int -> Int
gcdiv x y = maximum [x | x <-[1..a], a `mod` x == 0 && b `mod` x == 0]
    where
        a = max x y
        b = min x y

{ - b) Define and test using QuickCheck the following property: for x,y,z > 0, gcdiv of z*x and z*y is equal
to z multiplied by gcdiv of x and y. -}

p_div x y z = x > 0 && y > 0 && z > 0 ==> gcdiv (z*x) (z*y) == z * gcdiv x y

{ - c) By using this property that relates gcdiv and least common multiple (lcmul)
  gcdiv x y ∙ lcmul x y = x ∙ y
define a function to compute lcmul of two numbers:
lcmul 9 15 => 45
lcmul 30 75 => 150
Remark: Euclides’ algorithm is more efficient than the one you have developed in this exercise. -}

lcmul :: Int -> Int -> Int
lcmul x y = (x*y) `div` (gcdiv x y)



{ - 5. . Prime numbers
a) A prime number is a natural number with exactly two different positive divisors: 1 and p; hence, 1 is
not a prime number. Define a function isPrime to check whether a number is prime:
isPrime 7 => True
isPrime 10 => False -}

isPrime :: Int -> Bool
isPrime n = [x | x <-[1..n], n `mod` x == 0] == [1,n]

{ - b) Define, using list comprenhensions, a function primeUpTo returning a list with all primes numbers
whose value is less than or equal to function argument:
  primesUpTo 50 => [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47] -}

primesUpTo :: Int -> [Int]
primesUpTo n = [x | x <-[1..n], isPrime x]

{ - c) Provide a different definition (call it primesUpTo') by using predefined function filter instead of
a list comprenhension. -}

primesUpTo' :: Int -> [Int]
primesUpTo' n = filter isPrime [1..n]

{ - d) Test that both functions return same result with QuickCheck by using the following property:
 p1_primes x = primesUpTo x == primesUpTo' x 
Remark: Eratosthenes Sieve is a more efficient algorithm for this problem. -}



{ - 6. Predefined function zip takes two lists and returns a list of corresponding pairs. If one input list is
shorter, excess elements of the longer list are discarded:
  zip [1,2,3] [True,True,False] => [(1,True),(2,True),(3,False)]
  zip [1,2] [True,True,False] => [(1,True),(2,True)]
Predefined function take takes a natural number n and a list xs and returns a list with xs first n
elements:
  take 2 [7,3,1,2] => [7,3]
a) Fill in the following definition for this function (note that you cannot use the same name as the
predefined function):
so that computes same values as predefined function :
