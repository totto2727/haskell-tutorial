import Data.Function

type FirstName = String

type MiddleName = String

type LastName = String

data Name
  = Name
      { firstName :: FirstName,
        lastName :: LastName
      }
  | NameWithMiddle
      { firstName :: FirstName,
        middleName :: MiddleName,
        lastName :: LastName
      }
  | TwoInitialWithLast
      { firstInitial :: Char,
        middleInitial :: Char,
        lastName :: LastName
      }
  | FirstWithTwoInitial
      { firstName :: FirstName,
        middleInitial :: Char,
        lastInitial :: Char
      }

data Creator = AuthorCreator Author | ArtistCreator Artist

newtype Author = Author Name

data Artist = Person Name | Band String

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { pamphletName :: String,
    toyDescription :: String,
    toyPrice :: Double
  }

data Pamphlet = Pamphlet
  { pamphletTitle :: String,
    pamphletDescription :: String,
    contactInfo :: String
  }

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0

hpLovecraft :: Creator
hpLovecraft =
  AuthorCreator $
    Author
      TwoInitialWithLast
        { firstInitial = 'H',
          middleInitial = 'P',
          lastName = "Lovecraft"
        }

cthulhuMythos :: StoreItem
cthulhuMythos =
  BookItem
    Book
      { author = hpLovecraft,
        isbn = "aaaaaa",
        bookPrice = 30,
        bookYear = 1930,
        bookTitle = "Cthulhu Mythos"
      }

pamphlet :: StoreItem
pamphlet =
  PamphletItem $
    Pamphlet
      { pamphletTitle = "test",
        pamphletDescription = "test",
        contactInfo = "0000"
      }

newtype Circle = Circle {radius :: Double}

newtype Square = Square {side :: Double}

data Rectangle = Rectangle {vertical :: Double, horizontal :: Double}

data Shape = ShapeOfCircle Circle | ShapeOfSquare Square | ShapeOfRectangle Rectangle

perimeter :: Shape -> Double
perimeter (ShapeOfCircle c) = radius c * 2 * pi
perimeter (ShapeOfSquare s) = side s * 4
perimeter (ShapeOfRectangle r) = vertical r * 2 + horizontal r * 2

aria::Shape->Double
aria  (ShapeOfCircle c)=radius c **2 *pi
aria (ShapeOfSquare s)=side s**2
aria (ShapeOfRectangle r)=vertical r*horizontal r
