myTail :: [a] -> [a]
myTail (_ : xs) = xs
myTail [] = []

myHead :: [a] -> a
myHead (x : _) = x
myHead [] = error "No head for empty list"

myGCD :: Integral t => t -> t -> t
myGCD a 0 = a
myGCD a b =
  let remainder = a `mod` b
   in myGCD b remainder

myTake :: Int -> [a] -> [a]
myTake n list =
  let overListLength = length list - n
      reverseList = reverse list
      reverseDropList = drop overListLength reverseList
   in if overListLength < 0
        then list
        else reverse reverseDropList

myTake2 :: Int -> [a] -> [a]
myTake2 _ [] = []
myTake2 0 _ = []
myTake2 n (x : xs) = x : rest
  where
    rest = myTake2 (n -1) xs

myDrop :: (Eq t, Num t) => t -> [a] -> [a]
myDrop 0 list = list
myDrop _ [] = []
myDrop n (_ : xs) = myDrop (n -1) xs
