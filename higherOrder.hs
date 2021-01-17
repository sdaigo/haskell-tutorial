import Data.Char (toLower)

add3ToAll [] = []
add3ToAll (x : xs) = (3 + x) : add3ToAll xs

mul3ByAll [] = []
mul3ByAll (x : xs) = (3 * x) : mul3ByAll xs

addAnA [] = []
addAnA (x : xs) = ("a " ++ x) : addAnA xs

myMap _ [] = []
myMap fn (x : xs) = fn x : myMap fn xs

myFilter _ [] = []
myFilter test (x : xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

remove _ [] = []
remove test (x : xs) =
  if test x
    then remove test xs
    else x : remove test xs

myProduct xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^ 2) xs)

rcons x y = y : x

myReverse xs = foldl rcons [] xs

myElem x aList =
  length filteredList /= 0
  where
    filteredList = filter (== x) aList

isPalindrome text =
  loweredText == reverse loweredText
  where
    withoutSpace = filter (/= ' ') text
    loweredText = map toLower withoutSpace
