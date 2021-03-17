-- | LESSON 16

data AuthorName = AuthorName
    { firstName :: String
    , lastName  :: String
    }

data Book' = Book'
    { author' :: AuthorName
    , isbn'   :: String
    , title'  :: String
    , year'   :: Int
    , price'  :: Double
    }

-- data SportsCar = SportsCar Car Spoiler


type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          | FirstNameWithTwoInits FirstName Char Char
    deriving Show

data Creator = AuthorCreator Author | ArtistCreator Artist
    deriving Show
newtype Author = Author Name
    deriving Show
data Artist = Person Name | Band String
    deriving Show

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (
        Author (TwoInitialsWithLast 'H' 'P' "Lovecraft")
    )


data Book = Book
    { bookAuthor :: Creator
    , bookISBN   :: String
    , bookTitle  :: String
    , bookYear   :: Int
    , bookPrice  :: Double
    }

data VinylRecord = VinylRecord
    { recordArtist :: Creator
    , recordTitle  :: String
    , recordYear   :: Int
    , recordPrice  :: Double
    }

data CollectibleToy = CollectibleToy
    { toyName        :: String
    , toyDescription :: String
    , toyPrice       :: Double
    }

data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book)     = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy)       = toyPrice toy
price (PamphletItem _)    = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book)     = show (bookAuthor book)
madeBy (RecordItem record) = show (recordArtist record)
madeBy _                   = "unknown"


-- Q16.1 To further complicate the items in your store, you
-- eventually keep an inventory of free pamphlets. Pamphlets
-- have a title, a description, and a contact field for the
-- organization that provides the pamphlet. Create the
-- Pamphlet type and add it to StoreItem. Additionally,
-- modify the price so that it works with Pamphlet.
data Pamphlet = Pamphlet
    { pamphletTitle       :: String
    , pamphletDescription :: String
    , contact             :: String
    }


-- Q16.2 Create a Shape type that includes the following
-- shapes: Circle, Square, and Rectangle. Then write a
-- function to compute the perimeter of a Shape as well
-- as its area.
data Shape = Circle Double
           | Square Double
           | Rectangle Double Double

perimeter :: Shape -> Double
perimeter (Circle r)      = 2 * pi * r
perimeter (Square a)      = a * 4
perimeter (Rectangle a b) = (a + b) * 2

area :: Shape -> Double
area (Circle r)      = pi * r ^^ 2
area (Square a)      = a ^^ 2
area (Rectangle a b) = a * b
