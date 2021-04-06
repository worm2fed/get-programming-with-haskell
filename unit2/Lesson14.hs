data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6
    deriving (Eq, Ord)

instance Show SixSidedDie where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"


data TwoSidedDie = One | Two

-- show :: TwoSidedDie -> String
-- show One = "one"
-- show Two = "two"


-- instance Eq SixSidedDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _ _   = False


-- instance Ord SixSidedDie where
--     compare S6 S6 = EQ
--     compare S6 _  = GT
--     compare _ S6  = LT

--     compare S5 S5 = EQ
--     compare S5 _  = GT
--     compare _ S5  = LT

--     compare S4 S4 = EQ
--     compare S4 _  = GT
--     compare _ S4  = LT


data Test1 = AA | ZZ
    deriving (Eq, Ord)
data Test2 = ZZZ | AAA
    deriving (Eq, Ord)

instance Enum SixSidedDie where
    toEnum 0 = S1
    toEnum 1 = S2
    toEnum 2 = S3
    toEnum 3 = S4
    toEnum 4 = S5
    toEnum 5 = S6
    toEnum _ = error "No such value"

    fromEnum S1 = 0
    fromEnum S2 = 1
    fromEnum S3 = 2
    fromEnum S4 = 3
    fromEnum S5 = 4
    fromEnum S6 = 5


type Name' = (String, String)

names' :: [Name']
names' = [ ("Emil", "Cioran")
        , ("Eugene", "Thacker")
        , ("Friedrich", "Nietzsche")
        ]

newtype Name = Name (String, String)
    deriving (Show, Eq)

names :: [Name]
names = [ Name ("Emil", "Cioran")
          , Name ("Eugene", "Thacker")
          , Name ("Friedrich", "Nietzsche")
          ]

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) =
        compare (l1, f1) (l2, f2)


-- Q14.1 Note that Enum doesn’t require either Ord or Eq,
-- even though it maps types to Int values (which implement
-- both Ord and Eq). Ignoring the fact that you can easily
-- use deriving for Eq and Ord, use the derived
-- implementation of Enum to make manually defining Eq
-- and Ord much easier.
-- (==) a b = fromEnum a == fromEnum b
-- a `compare` b = fromEnum a `compare` fromEnum b


-- Q14.2 Define a five-sided die (FiveSidedDie type). Then
-- define a type class named Die and at least one method
-- that would be useful to have for a die. Also include
-- superclasses you think make sense for a die. Finally,
-- make your FiveSidedDie an instance of Die.
data FiveSidedDie = I | II | III | IV | V
    deriving (Show, Eq, Ord)

class (Eq a, Ord a) => Die a where
    bingo :: a -> a -> Bool

instance Die FiveSidedDie where
    bingo a b = a == b