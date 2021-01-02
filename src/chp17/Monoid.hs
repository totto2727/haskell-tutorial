import Data.Function
import Data.List

{-myLast :: [a] -> a
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
myAny1 checker list = map checker list & foldr (||) False-}

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f l1 l2 = zipWith f newL1 cycledL2
  where
    l2Length = length l2
    repeatedL1 = map (replicate l2Length) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

newtype Events = Events [String] deriving (Show)

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

instance Semigroup Events where
  (<>) (Events e1) (Events [])=Events e1
  (<>) (Events []) (Events e2)=Events e2 
  (<>) (Events e1) (Events e2) = Events $ cartCombine combiner e1 e2
    where
      combiner x y = mconcat [x, "-", y]

newtype Probs = Probs [Double] deriving (Show)

instance Semigroup Probs where
  (<>) (Probs p1) (Probs [])=Probs p1
  (<>) (Probs []) (Probs p2)=Probs p2
  (<>) (Probs p1) (Probs p2) = Probs $ cartCombine (*) p1 p2
  
instance Monoid Probs where
  mempty=Probs []
  mappend =(<>)

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
  where
    totalProbs = sum probs
    normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair events probs = mconcat [events, "|", show probs, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where
      pairs = zipWith showPair events probs

coinTossTable :: PTable
coinTossTable = createPTable (Events ["front", "back"]) (Probs [1, 1])

{-combineEvents :: Events -> Events -> Events
combineEvents = cartCombine combiner
  where
    combiner x y = mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine combiner
  where
    combiner x y = x * y-}

instance Semigroup PTable where
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = mconcat [e1,e2]
      newProbs = mconcat [p1,p2]

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

events1 :: Events
events1=Events ["a","b","c"]
events2 :: Events
events2=Events ["d","e","f"]
probs1 :: Probs
probs1=Probs [1,2,3]
probs2 :: Probs
probs2=Probs [4,5,6]