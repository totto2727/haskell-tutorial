cup :: t1 -> (t1 -> t2) -> t2
cup flOz message = message flOz

getOz :: ((p -> p) -> t) -> t
getOz aCup = aCup id

drink :: (Ord t1, Num t1) => ((p -> p) -> t1) -> t1 -> (t1 -> t2) -> t2
drink aCup ozDrank =
  if ozDrank <= flOz
    then cup (flOz - ozDrank)
    else cup 0
  where
    flOz = getOz aCup

isEmpty :: (Eq a, Num a) => ((p -> p) -> a) -> Bool
isEmpty aCup= getOz aCup ==0

coffeeCup :: (Integer -> t2) -> t2
coffeeCup=cup 12

afterManySips :: (Integer -> Integer) -> Integer
afterManySips =foldl drink coffeeCup [1,1,1,1,1] 

