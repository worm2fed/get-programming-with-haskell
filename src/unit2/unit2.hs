-- | LESSON 11

x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1, 2, 3]

testScores :: [Double]
testScores = [0.99, 0.7, 0,8]

letters :: [Char]
letters = ['a', 'b', 'c']

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')

streetAddress :: (Int, String)
streetAddress = (123, "Happy St.")


double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = fromIntegral n / 2

halve :: Int -> Int
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n * 2)

anotherNumber :: Int
anotherNumber = read "6"

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n


simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

nameTriple = makeTriple "Oscar" 'D' "Grouch"


-- Q11.1 What is the type signature for filter?
-- How is it different from map?
-- filter :: (a -> Bool) -> [a] -> [a]


-- Q11.2 In Haskell, both tail and head have an error when
-- called on an empty list. You can write a version of tail
-- that won’t fail but instead return an empty list when
-- called on an empty list. Can you write a version of head
-- that returns an empty list when called on an empty list?
-- To answer this, start by writing out the type signatures
-- of both head and tail.
-- tail :: [a] -> [a]
-- head :: [a] -> a
-- no


-- Q11.3 Recall myFoldl from lesson 9.
-- myFoldl f init [] = init
-- myFoldl f init (x:xs) = myFoldl f newInit xs
--    where newInit = f init x
-- What’s the type signature of this function?
-- Note: foldl has a different type signature.
-- myFoldl :: (b -> a -> b) -> b -> [a] -> b



-- | LESSON 12

patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
    where name = lname ++ ", " ++ fname
          ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type FirstName = String
type LastName = String
type Age = Int
type Height = Int

-- patientInfo :: FirstName -> LastName -> Age -> Height -> String

type PatientName = (FirstName, LastName)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' name age height = prepareName ++ " " ++ prepareAgeHeight
    where prepareName = firstName name ++ ", " ++ lastName name
          prepareAgeHeight = "(" ++ show age ++ "yrs, " ++ show height ++ "in)"


data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male   = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType


patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _               = True
canDonateTo _ (BloodType AB _)              = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _                             = False

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l)             = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"


data Patient = Patient Name Sex Int Int Int BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: Patient
janeESmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 25 70 150 (BloodType O Pos)

getName :: Patient -> Name
getName (Patient n _ _ _ _ _) = n

getAge :: Patient -> Int
getAge (Patient _ _ a _ _ _) = a

getBloodType :: Patient -> BloodType
getBloodType (Patient _ _ _ _ _ bt) = bt

data Patient' = Patient' { name      :: Name
                         , sex       ::  Sex
                         , age       :: Int
                         , height    :: Int
                         , weight    :: Int
                         , bloodType :: BloodType
                         }

jackieSmith :: Patient'
jackieSmith = Patient' { name = Name "Jackie" "Smith"
                       , age = 43
                       , sex = Female
                       , height = 62
                       , weight = 115
                       , bloodType = BloodType O Neg
                       }

-- showName (name jackieSmith)

jackieSmithUpdated = jackieSmith { age = 44 }


-- Q12.1 Write a function similar to canDonateTo that takes
-- two patients as arguments rather than two BloodTypes.
canDonateTo' :: Patient' -> Patient' -> Bool
canDonateTo' a b               = canDonateTo (bloodType a) (bloodType b)


-- Q12.2 Implement a patientSummary function that uses your
-- final Patient type. patient- Summary should output a
-- string that looks like this:
--  **************
--  Patient Name: Smith, John
--  Sex: Male
--  Age: 46
--  Height: 72 in.
--  Weight: 210 lbs.
--  Blood Type: AB+
--  **************
-- If you need to, feel free to create useful helper functions.
patientSummary :: Patient -> String
patientSummary (Patient name sex age height weight blood) =
        "\nPatient name: " ++ showName name  ++
        "\nSex: " ++ showSex  sex ++
        "\nAge: " ++ show age ++
        "\nHeight: " ++ show height ++
        "\nWeight: " ++ show weight ++
        "\nBlood Type: " ++ showBloodType blood
    where showSex Male   = "Male"
          showSex Female = "Female"



-- | LESSON 13

aList :: [String]
aList = ["cat", "dog", "mouse"]


addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2


class TypeName a where
    fun1 :: a -> a
    fun2 :: a -> String
    fun3 :: a -> a -> Bool

class Describable a where
    describe :: a -> String


data Icecream = Chocolate | Vanilla
    deriving (Show, Eq, Ord)


-- Q13.1 If you ran the :info examples, you likely noticed
-- that the type Word has come up a few times. Without
-- looking at external resources, use :info to explore Word
-- and the relevant type classes to come up with your own
-- explanation for the Word type.
-- How is it different from Int?
-- minBound :: Word => 0


-- Q13.2 One type class we didn’t discuss is Enum. Use :info
-- to look at the definition of this type class, as well as
-- example members. Now consider Int, which is an instance
-- of both Enum and Bounded. Given the following definition of inc:
inc :: Int -> Int
inc x = x + 1
-- and the succ function required by Enum, what’s the
-- difference between inc and succ for Int?
-- succ (maxBound :: Int) =>
--      *** Exception: Prelude.Enum.succ{Int}: tried to
--      take `succ' of maxBound
-- inc (maxBound :: Int) => -9223372036854775808


-- Q13.3 Write the following function that works just like
-- succ on Bounded types but can be called an unlimited number
-- of times without error. The function will work like inc in
-- the preceding example but works on a wider range of types, including types that aren’t members of Num
-- Your definition will include functions/values from
-- Bounded, Enum, and the mystery type class. Make a note
-- of where each of these three (or more) functions/values
-- comes from.
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n



-- | LESSON 14

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

newtype Name'' = Name'' (String, String)
    deriving (Show, Eq)

names'' :: [Name'']
names'' = [ Name'' ("Emil", "Cioran")
          , Name'' ("Eugene", "Thacker")
          , Name'' ("Friedrich", "Nietzsche")
          ]

instance Ord Name'' where
    compare (Name'' (f1, l1)) (Name'' (f2, l2)) =
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



-- | LESSON 15

data FourLetterAlphabet = L1 | L2 | L3 | L4
    deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum c + halfAlphabet
          rotation = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar = rotN (1 + largestCharNumber)

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map rot4l
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot4l = rotN alphaSize


data ThreeLetterAlphabet = Alpha | Beta | Kappa
    deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha, Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder = map rot3l
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3l = rotN alphaSize

rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder n c = toEnum rotation
    where halfN = n `div` 2
          offset = if even n
                   then fromEnum c + halfN
                   else 1 + fromEnum c + halfN
          rotation = offset `mod` n

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder = map rot3lDecoder
    where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
          rot3lDecoder = rotNDecoder alphaSize


rotEncoder :: String -> String
rotEncoder = map rotChar
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
    where alphaSize = 1 + fromEnum (maxBound :: Char)
          rotCharDecoder = rotNDecoder alphaSize


xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && not (v1 && v2)

xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)


type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0
               then False : intToBits' nextVal
               else True  : intToBits' nextVal
    where remainder = n `mod` 2
          nextVal   = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (intToBits' n)
          missingBits = maxBits - length reversedBits
          leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
    where size = length bits
          indices = [size - 1, size - 2 .. 0]
          trueLocations = filter (\x -> fst x == True)
                            (zip bits indices)
bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)


myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText =
    map (\(v1, v2) -> v1 `xor` v2) (zip padBits plainTextBits)
    where padBits = map charToBits pad
          plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar bitList
    where bitList = applyOTP' pad plainText

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad


class Cipher a where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot
instance Cipher Rot where
    encode Rot = rotEncoder
    decode Rot = rotDecoder

newtype OneTimePad = OTP String
instance Cipher OneTimePad where
    encode (OTP pad) = applyOTP pad
    decode = encode

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])


prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a * seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

prngPad :: (Int -> Int) -> Int -> OneTimePad
prngPad gen seed = OTP (map toEnum (iterate gen seed))

data Stream = Stream (Int -> Int) Int
instance Cipher Stream where
    encode (Stream gen seed) = encode (prngPad gen seed)
    decode = encode
