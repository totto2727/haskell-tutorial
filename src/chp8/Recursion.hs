myLength :: Num p => [a] -> p
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

--finiteCycle :: [a] -> [a]
--finiteCycle (x : xs) = x : xs ++ [x]

--myCycle :: [a] -> [a]
--myCycle (x : xs) = x : myCycle (xs ++ [x])

ackerman :: (Num a, Num t, Eq a, Eq t) => a -> t -> t
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m -1) 1
ackerman m n = ackerman (m -1) (ackerman m (n -1))

collatz :: (Integral t, Integral a) => t -> a
collatz 1 = 1
collatz n =
  if even n
    then 1+collatz (n `div` 2)
    else 1+collatz (n * 3)
    

myReverse :: [a] -> [a]
myReverse []=[]
myReverse (x:xs)=myReverse xs ++ [x]

fastFib :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t1 -> t2
fastFib _ x2 0 =x2
fastFib x1 x2 counter=fastFib x2 (x1+x2) (counter-1)   