import           Data.Char (isDigit)
myTake :: Int -> [a] -> [a]
myTake 0 _  = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _      = []
myTakePM n (x:xs) = x : myTake (n - 1) xs

-- the missing pattern is: myTakePM _ [] = []

myHead :: [a] -> a
myHead []    = error "empty list"
myHead (x:_) = x

-- maximum => error on empty list
-- succ => error on maxBound value
-- sum => error on infinite list

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n - 1) (Just $ tail xs)


primes :: [Int]
primes = [2, 3, 5, 7]

maxN :: Int
maxN = 10

isPrime' :: Int -> Maybe Bool
isPrime' n
  | n < 2 = Nothing
  | n > maxN = Nothing
  | otherwise = Just $ n `elem` primes

eitherHead :: [a] -> Either String a
eitherHead []     = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1, 2, 3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

-- (+) <$> eitherHead intExample <*> eitherHead (tail intExample)

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right $ n `elem` primes

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge     = "Value exceeds max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

displayResult :: Either PrimeError Bool -> String
displayResult (Right True)      = "It's prime"
displayResult (Right False)     = "It's composite"
displayResult (Left primeError) = show primeError

main :: IO ()
main = do
  print "Enter a number to test for primality"
  n <- read <$> getLine
  let result = isPrime n
  print $ displayResult result


-- Q38.1 Make a function addStrInts that takes two Ints
-- represented as Strings and adds them. The function
-- would return an Either String Int. The Right constructor
-- should return the result, provided that the two arguments
-- can be parsed into Ints (use Data.Char isDigit to check).
-- Return a different Left result for the three possible cases:
--    - First value can’t be parsed.
--    - Second value can’t be parsed.
--    - Neither value can be parsed.
data ParseIntError = WrongFirstValue
                   | WrongSecondValue
                   | WrongBothValues
  deriving Show

isStrInt :: String -> Bool
isStrInt = all isDigit

addStrInts :: String -> String -> Either ParseIntError Int
addStrInts a b
  | isStrInt a && isStrInt b = Right $ read a + read b
  | not (isStrInt a) && isStrInt b = Left WrongFirstValue
  | isStrInt a && not (isStrInt b) = Left WrongSecondValue
  | otherwise = Left WrongBothValues


-- Q38.2 The following are all partial functions. Use the
-- type specified to implement a safer version of the function:
--    - succ—Maybe
--    - tail—[a] (Keep the type the same.)
--    - last—Either (last fails on empty lists and infinite
--        lists; use an upper bound for the infinite case.)
succMaybe :: (Eq a, Bounded a, Enum a) => a -> Maybe a
succMaybe x
  | maxBound == x = Nothing
  | otherwise = Just $ succ x

maybeTail :: [a] -> Maybe [a]
maybeTail []     = Nothing
maybeTail (_:xs) = Just xs

eitherLast :: [a] -> Either String a
eitherLast [] = Left "empty list"
eitherLast xs = helper 1000 xs
  where
    helper 0 _      = Left "exceed max bound"
    helper _ [x]    = Right x
    helper n (x:xs) = helper (n - 1) xs
