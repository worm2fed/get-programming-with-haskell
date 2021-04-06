import           Data.Char (toLower)


addAnA []     = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

squareAll []     = []
squareAll (x:xs) = x ^ 2 : squareAll xs

myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs


myFilter _ [] = []
myFilter test (x:xs) = if test x
                       then x : myFilter test xs
                       else myFilter test xs

remove _ [] = []
remove test (x:xs) = if test x
                       then remove test xs
                       else x : remove test xs


myProduct xs = foldl (*) 1 xs

sumOfSquares xs = foldl (+) 0 (map (^ 2) xs)

rcons x y = y : x
myReverse' xs = foldl rcons [] xs

myFoldl _ init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x

myFoldr _ init [] = init
myFoldr f init (x:xs) = f x rightResult
    where rightResult = myFoldr f init xs



-- Q9.1 Use filter and length to re-create the elem function.
myElem x xs = length (filter (== x) xs) /= 0


-- Q9.2 Your isPalindrome function from lesson 6 doesn’t
-- handle sentences with spaces or capitals. Use map and
-- filter to make sure the phrase
-- “A man a plan a canal Panama”
-- is recognized as a palindrome.
-- isPalindrome' word = word == reverse word
isPalindrome' word = lowerNoSpaces == reverse lowerNoSpaces
    where noSpaces = filter (/= ' ') word
          lowerNoSpaces = map toLower noSpaces


-- Q9.3 In mathematics, the harmonic series is the sum of
-- 1/1 + 1/2 + 1/3 + 1/4 ....
-- Write a function harmonic that takes an argument n and
-- calculates the sum of the series to n. Make sure to use
-- lazy evaluation.
harmonic n = sum (take n series)
    where series = map (1 /) [1..]
