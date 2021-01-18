cup flOz = \_ -> flOz
cup flOz = \message -> message flOz

getOz aCup = aCup id

drink aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    ozDiff = flOz - ozDrank
    flOz = getOz aCup

isEmpty aCup = getOz aCup == 0

coffeeCup = cup 12

afterManySips = foldl drink coffeeCup [1, 1, 1, 1, 1]