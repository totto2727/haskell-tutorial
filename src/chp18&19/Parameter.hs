import qualified Data.Map as Map
import Data.Maybe

newtype Box a = Box a deriving (Show)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box n) = Box $ f n

data Triple a = Triple a a a deriving (Show)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

organIds :: [Int]
organIds = [1, 2, 3, 4, 5, 6]

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

organPairs :: [(Int, Organ)]
organPairs = zip organIds organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Organ] -> [Int]
organCounts organList = map counter allOrgans
  where
    counter target = length $ filter (== target) organList

organInventory :: [Organ] -> Map.Map Organ Int
organInventory = Map.fromList . zip allOrgans . organCounts

--19
possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog =
  let getItem = (`Map.lookup` catalog)
   in map getItem ids

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan target = length . filter (== Just target)

isSomething :: Maybe a -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justSomething :: [Maybe a] -> [Maybe a]
justSomething = filter isSomething

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a organ"
  show (Bag organ) = show organ ++ " in a organ"

data Location = Lab | Kitchen | Bathroom deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat o) = (Lab, Vat o)
placeInLocation (Cooler o) = (Lab, Cooler o)
placeInLocation (Bag o) = (Kitchen, Bag o)

{-
process :: Organ -> (Location, Container)
process = placeInLocation . organToContainer

report :: (Location, Container) -> String
report (location, catalog) = show catalog ++ " in the " ++ show location

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report $ process organ
processAndReport Nothing = "Error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest organId = processAndReport . Map.lookup organId
-}

process :: Maybe Organ -> Maybe (Location, Container)
process (Just organ)= Just . placeInLocation . organToContainer $ organ
process Nothing=Nothing

report :: Maybe (Location, Container) -> String
report (Just (location, catalog)) = show catalog ++ " in the " ++ show location
report Nothing="Error,container not found"

processAndReport :: Maybe Organ -> String
processAndReport = report . process

processRequest::Int->Map.Map Int Organ->String
processRequest catalogId=processAndReport . Map.lookup catalogId

emptyDrawers::[Maybe Organ]->Int
emptyDrawers=length . filter isNothing 

maybeMap::(a->b)->Maybe a->Maybe b
maybeMap _ Nothing =Nothing
maybeMap f (Just v)=Just . f $ v