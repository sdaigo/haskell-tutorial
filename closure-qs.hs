-- Q5-1

inc x = x + 1

double x = x * 2

square x = x ^ 2

ifEven fn x = if even x then fn x else x

ifEvenInc = ifEven inc

ifEvenDouble = ifEven double

ifEvenSquare = ifEven square

-- Q5-2
binaryPartialApplication binaryFn arg =
  \x -> binaryFn arg x
