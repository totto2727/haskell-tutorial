repeatOriginal :: a -> [a]
repeatOriginal n=cycle [n]

subSeq :: Int -> Int -> [a] -> [a]
subSeq start end list=
  let takeByEndList = take end list
  in drop start takeByEndList

inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf x list=
  let halfListLength=length list `div` 2
      halfList=take  halfListLength list
  in elem x halfList