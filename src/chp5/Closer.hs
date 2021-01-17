ifEven :: Integral p => (p -> p) -> p -> p
ifEven func x =
  if even x
    then func x
    else x

genIfXEven :: Integral p => p -> (p -> p) -> p
genIfXEven x = (`ifEven` x)

genIf6Even :: (Integer -> Integer) -> Integer
genIf6Even = genIfXEven 6

if6EvenDouble :: Integer
if6EvenDouble = genIf6Even (* 2)

getRequestUrl :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestUrl host apiKey resource rid = host ++ "/" ++ resource ++ "/" ++ rid ++ "?token=" ++ apiKey

{-
genHostUrlBuilder :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
genHostUrlBuilder host = \ apikey resource rid -> getRequestUrl host apikey resource rid

genApiRequestBuilder :: (t1 -> t2 -> t3 -> t4) -> t1 -> t2 -> t3 -> t4
genApiRequestBuilder hostBuilder apikey=(\resource rid -> hostBuilder apikey resource rid)

genResourceRequestBuilder :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
genResourceRequestBuilder apiRequestBuilder resource=(\rid->apiRequestBuilder resource rid)
-}

exampleUrlBuilder :: [Char] -> [Char] -> [Char] -> [Char]
--exampleUrlBuilder = genHostUrlBuilder "http://example.com"
exampleUrlBuilder = getRequestUrl "http://example.com"

myExampleUrlBuilder :: [Char] -> [Char] -> [Char]
myExampleUrlBuilder = exampleUrlBuilder "1337hAsk311"

myExampleResourceUrlBuilder :: [Char] -> [Char]
myExampleResourceUrlBuilder = myExampleUrlBuilder "book"

subtract2 :: Integer -> Integer
subtract2 = flip (-) 2
   
ifEvenInc :: Integer -> Integer
ifEvenInc = ifEven (+ 1)

ifEvenDouble :: Integer -> Integer
ifEvenDouble = ifEven (* 2)

ifEvenSquare :: Integer -> Integer
ifEvenSquare = ifEven (^ (2::Int))

binaryPartialApplication :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
binaryPartialApplication func x=(\y->func x y)

subtractFrom3 :: Integer -> Integer
subtractFrom3=binaryPartialApplication (-) 3

