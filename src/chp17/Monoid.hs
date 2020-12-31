import Data.Function
import Data.List

myLast :: [a] -> a
myLast = head . reverse

myLast1 :: [a] -> a
myLast1 list = reverse list & head

myMin :: Ord a => [a] -> a
myMin = head . sort

myMin1 :: Ord a => [a] -> a
myMin1 list = sort list & head

myMax :: Ord a => [a] -> a
myMax = head . reverse . sort

myMax1 :: Ord a => [a] -> a
myMax1 list = sort list & reverse & head

myAll :: (a -> Bool) -> [a] -> Bool
myAll checker = foldl (&&) True . map checker

myAll1 :: (a -> Bool) -> [a] -> Bool
myAll1 checker list =
  map checker list
    & foldl (&&) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny checker = foldr (||) False . map checker

myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 checker list = map checker list & foldr (||) False

type Events = [String]

type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where
    totalProbs = sum probs
    normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair events probs = mconcat [events, "|", show probs, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where
      pairs = zipWith showPair events probs

coinTossTable :: PTable
coinTossTable = createPTable ["front", "back"] [1, 1]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f l1 l2 = zipWith f newL1 cycledL2
  where
    l2Length = length l2
    repeatedL1 = map (replicate l2Length) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents = cartCombine combiner
  where
    combiner x y = mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine combiner
  where
    combiner x y = x * y

instance Semigroup PTable where
  (<>) (PTable [] []) pTable2 = pTable2
  (<>) pTable1 (PTable [] []) = pTable1
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable [] []
  mappend = (<>)
