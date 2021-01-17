calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

fn n =
  if even n
    then n - 2
    else 3 * n + 1

sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + 2) ^ 2

doubleDouble x =
  dubs * 2
  where
    dubs = x * 2

doubleDouble2 x = (\dubs -> dubs * 2) (x * 2)
