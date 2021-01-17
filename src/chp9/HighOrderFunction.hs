import Data.Char

--myProduct :: (Foldable t, Num b) => t b -> b
--myProduct = foldl (*) 1

removeChecker :: Eq a => a -> a -> Bool
removeChecker x y = x /= y

remove :: Eq a => a -> [a] -> [a]
remove x = filter (x /=)

myElem :: Eq a => a -> [a] -> Bool
myElem x list = [] /= filteredList
  where
    filteredList = filter (x ==) list

isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == reverse list

isPalindromeAdv :: [Char] -> Bool
isPalindromeAdv str =
  let filteredStr = filter (/= ' ') str
      loweredStr = map toLower filteredStr
   in (==) loweredStr (reverse loweredStr)

harmonic :: (Enum p, Fractional p) => Int -> p
harmonic 0 = 0
harmonic n = fraction + harmonic (n-1)
  where
    xs = [1,2..]
    fraction = 1 / xs !! (n-1)
    
harmonic2 :: (Enum a, Fractional a) => Int -> a
harmonic2 n = sum (take n seriesValue)
  where seriesPairs=zip (repeat 1.0) [1.0,2.0..] 
        seriesValue=map (uncurry (/)) seriesPairs
