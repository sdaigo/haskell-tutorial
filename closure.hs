inc x = x + 1

double x = x * 2

square x = x ^ 2

ifEven fn x =
  if even x
    then fn x
    else x

-- ifEvenInc n = ifEven inc n
-- ifEvenDouble n = ifEven double n
-- ifEvenSquare n = ifEven square n

genIfEven f = \x -> ifEven f x

ifEvenInc = genIfEven inc

-- x を使ってクロージャーを作り新しい関数を返す
-- 返された関数は、xが偶数の場合にxに適用する関数として渡すことができる
genIfXEven x = \f -> ifEven f x

getRequestUrl host apiKey resource id =
  host ++ "/"
    ++ resource
    ++ "/"
    ++ id
    ++ "?token="
    ++ apiKey

genHostRequestBuilder host =
  \apikey resource id ->
    getRequestUrl host apikey resource id

genApiRequestBuilder hostBuilder apikey =
  \resource id ->
    hostBuilder apikey resource id

add4 a b c d = a + b + c + d

mystery = add4 3

-- mystery 2 3 4
-- > 12

-- mystery 5 6 7
-- > 21

exampleUrlBuilder = getRequestUrl "http://example.com"

myExampleUrlBuilder = exampleUrlBuilder "1337hAsk3ll"

-- myExampleUrlBuilder "book" "1227"
-- -> "http://example.com/book/1227?token=1337hAsk3ll"

bookRequestBuilder = getRequestUrl "http://example.com" "1337hAsk3ll" "book"

-- > bookRequestBuilder "12223"
-- -> "http://example.com/book/12223?token=1337hAsk3ll"

flipBinaryArgs binaryFunction =
  \x y -> binaryFunction y x
