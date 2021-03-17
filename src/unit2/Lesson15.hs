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
