halve ::Integer->Integer
halve a=a `div` 2

printDouble::Integer->String
printDouble a=show (a*2)

makeAddress::Integer->String->String->(Integer,String,String)
makeAddress number stream town=(number,stream,town)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []=[]
myFilter f (x:xs)=
  if f x
  then x:x : myFilter f xs
  else myFilter f xs

myTail::[a]->[a]
myTail []=[]
myTail (_:xs)=xs
