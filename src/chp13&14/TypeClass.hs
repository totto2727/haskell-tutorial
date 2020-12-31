inc :: Int -> Int
inc n = n + 1

class Describable a where
  describe :: a -> String

{-data SixSideDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Ord, Eq, Enum)

instance Show SixSideDie where
  show S1 = "one"
  show S2 = "two"
  show S3 = "three"
  show S4 = "four"
  show S5 = "five"
  show S6 = "six"-}

newtype Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

data ThreeSideDie = Side1 | Side2 | Side3 deriving (Enum, Eq, Show)

class Enum a => Die a where
  roll :: Int -> a
  square :: a -> Int

instance Die ThreeSideDie where
  roll n = toEnum (div n 3) :: ThreeSideDie
  square n = fromEnum n ^ (2 :: Int)
