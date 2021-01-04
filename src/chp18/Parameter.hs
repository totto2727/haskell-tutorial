import qualified Data.Map as Map

newtype Box a = Box a deriving (Show)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box n) = Box $ f n

data Triple a = Triple a a a deriving (Show)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

ids :: [Int]
ids = [1, 2, 3, 4, 5, 6]

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Organ] -> [Int]
organCounts organList = map counter allOrgans
  where
    counter countOrgan = length $ filter (== countOrgan) organList

organInventory :: [Organ] -> Map.Map Organ Int
organInventory = Map.fromList . zip allOrgans . organCounts
