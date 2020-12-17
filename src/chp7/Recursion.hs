myTail :: [a] -> [a]
myTail(_:xs)=xs
myTail []=[]

myHead :: [a] -> a
myHead(x:_)=x
myHead []=error "No head for empty list"

myGCD :: Integral t => t -> t -> t
myGCD a 0 =a
myGCD a b =
  let remainder=a`mod`b
  in myGCD b remainder
  
  