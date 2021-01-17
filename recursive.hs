{-
  最大公約数を求める
  1. a=20, b=16
  2. a/b = 20/16 = 1 ... 4
  3. a=16, b=4
  4. a/b = 16/4 = 4 ... 0
  5. GCD = b = 4
-}
myGcd a b =
  if remainder == 0
    then b
    else myGcd b remainder
  where
    remainder = a `mod` b

sayAmount n = case n of
  1 -> "one"
  2 -> "two"
  _ -> "a bunch"

myHead (x : _) = x

myTail (_ : xs) = xs
myTail [] = []

myGcdWithPatternMatch a 0 = a
myGcdWithPatternMatch a b = myGcdWithPatternMatch b (a `mod` b)

myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) =
  x : rest
  where
    rest = myTake (n - 1) xs

finiteCycle (first : rest) = first : rest ++ [first]

myCycle (first : rest) = first : myCycle (rest ++ [first])

ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))

collatz 1 = 1
collatz n =
  if even n
    then 1 + collatz (n `div` 2)
    else 1 + collatz (n * 3 + 1)

myReverse [] = []
myReverse (x : []) = [x]
myReverse (first : rest) =
  myReverse rest ++ [first]

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n -2)

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c =
  fastFib (x + y) x (c - 1)