calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

square n = n ^ n

double n = n * 2

f n =
  if even n
    then n -2
    else nTriple + 1
  where
    nTriple = n * 3