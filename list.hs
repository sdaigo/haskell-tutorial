-- Q6-1

-- my repeat function

myRepeat x = cycle [x]

subseq start end aList =
  take diff (drop start aList)
  where
    diff = end - start

inFirstHalf x aList =
  x `elem` firstHalf
  where
    middle = length aList `div` 2
    firstHalf = take middle aList