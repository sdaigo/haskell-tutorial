x :: Int
x = 2

y :: Integer
y = 2

values :: [Int]
values = [1, 2, 3]

pet :: [Char]
pet = "cat"

pet2 :: String
pet2 = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")

double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = fromIntegral n / 2

halve :: Integer -> Integer
halve n = n `div` 2

z = read "6"

q = z / 2 -- zを扱う方法をコンパイラに与えることで↑でDoubleと推論してくれる

printDouble :: Int -> String
printDouble n = show (n * 2)

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

{--
in Haskell

makeAddress number street town = (number, street, town)

in JavaScript

const makeAddress = number => street => town => [number, street, town]
--}

ifEven :: (Int -> Int) -> Int -> Int
ifEven fn x =
  if even x
    then fn x
    else x

-- 型変数

simple :: x -> x
simple x = x

safeTail :: [x] -> [x]
safeTail [] = []
safeTail (_ : xs) = xs
