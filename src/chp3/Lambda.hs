body :: (Num a, Ord a) => a -> a -> a
body x y =
  ( \sumSquare squareSum ->
      if sumSquare > squareSum
        then sumSquare
        else squareSum
  )
    (x * x + y * y)
    ((x + y) * 2)

letBody :: (Ord p, Num p) => p -> p -> p
letBody x y =
  let sumSquare = x^(2::Int) + y ^ (2::Int)
      squareSum = (x+y)^(2::Int)
   in if sumSquare > squareSum
        then sumSquare
        else squareSum
        
doubleLambda :: Num a => a -> a
doubleLambda = (* 2)

doubleDouble :: Num a => a -> a
doubleDouble x =(* 2) x*2

{-overwrite x = 
  (\x->
    (\x->
      (\x->x)x+1
    )x+1
  )x-} 

