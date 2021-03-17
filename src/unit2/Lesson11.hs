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