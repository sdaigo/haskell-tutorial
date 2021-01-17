import Data.List ()

-- ifEvenInc n =
--   if even n
--     then n + 1
--     else n

-- ifEvenDouble n =
--   if even n
--     then n * 2
--     else n

-- ifEvenSquare n =
--   if even n
--     then n ^ 2
--     else n

-- それぞれの関数は振る舞いだけ異なる
-- increment, double, square

-- 共通する`if even`というパターンを抽象化する
ifEven fn x =
  if even x
    then fn x
    else x

-- それぞれの振る舞いを抜き出す
increment n = n + 1

double n = n * 2

square n = n ^ 2

ifEvenInc n = ifEven increment n

ifEvenDouble n = ifEven double n

ifEvenSquare n = ifEven square n

names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Mark", "Hook"),
    ("Stephen", "Morris")
  ]

compareLastName n1 n2 =
  if lastName1 > lastName2
    then GT
    else
      if lastName1 < lastName2
        then LT
        else
          if firstName1 > firstName2
            then GT
            else
              if firstName1 < firstName2
                then LT
                else EQ
  where
    lastName1 = snd n1
    lastName2 = snd n2
    firstName1 = fst n1
    firstName2 = fst n2

addressLetter name location =
  nameText ++ " - " ++ location
  where
    nameText = fst name ++ " " ++ snd name