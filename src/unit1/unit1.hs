-- | LESSON 2

import           Data.Char (toLower)
simple x = x


calcChange owed given = if given - owed > 0
                        then given - owed
                        else 0

calcChange' owed given = if change > 0
                         then change
                         else 0
    where change = given - owed


doublePlusTwo x = doubleX + 2
    where doubleX = x * 2


-- let x = simple simple
-- let x = 6
-- the final result of x will be 6


-- Q2.1 You used Haskell’s if then else expression to write
-- calcChange. In Haskell, all if statements must include
-- an else component. Given our three rules for functions,
-- why can’t you have an if statement all by itself?

-- because we have to return something


-- Q2.2 Write functions named inc, double, and square that
-- increment, double, and square an argument n, respectively
inc x = x + 1
double x = x * 2
square x = x * x


-- Q2.3 Write a function that takes a value n. If n is even,
-- the function returns n - 2, and if the number is odd, the
-- function returns 3 × n + 1. To check whether the number
-- is even, you can use either Haskell’s even function or
-- mod (Haskell’s modulo function).
checkN n = if even n
    then n - 2
    else 3 * n + 1



-- | LESSON 3

-- (\x -> x * 2) 3
-- (\x -> x * 2) 9


sumSquareOrSquareSum x y = if sumSquare > squareSum
                           then sumSquare
                           else squareSum
    where sumSquare = x ^ 2 + y ^ 2
          squareSum = (x + y) ^ 2

sumSquareOrSquareSum' x y = if (x ^ 2 + y ^ 2) > ((x + y) ^ 2)
                           then x ^ 2 + y ^ 2
                           else (x + y) ^ 2

body sumSquare squareSum = if sumSquare > squareSum
                           then sumSquare
                           else squareSum

sumSquareOrSquareSum'' x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

-- body' = (\sumSquare squareSum ->
--         if sumSquare > squareSum
--         then sumSquare
--         else squareSum)


doubleDouble x = dubs * 2
    where dubs = x * 2

doubleDouble' x = (\a -> a * 2) (x * 2)


sumSquareOrSquareSum''' x y =
    let sumSquare = x ^ 2 + y ^ 2
        squareSum = (x + y) ^ 2
    in if sumSquare > squareSum
       then sumSquare
       else squareSum


overwrite x =
    let x = 2
    in  let x = 3
        in let x = 4
           in  x

-- overwrite' x =
--     (x ->
--         (\x ->
--             (\x -> x) 4
--         ) 3
--     ) 2


-- Q3.1 Practice writing lambda functions by rewriting each
-- function in lesson 3 as a lambda expression

-- all functions already presented in lambda form...


-- Q3.2 Using a let expression and a lambda function aren’t
-- exactly the same thing under the hood. For example, the
-- following code will cause an error if you try to run it:
--  counter x = let x = x + 1
            --  in
--               let x = x + 1
--                in
--                 x
-- To prove that let and lambda aren’t identical, rewrite
-- the counter function exactly as it is here, but use
-- nested lambdas instead of let.
-- (Hint: Start at the end.)

counter x =
    (\x -> x + 1)
        ((\x -> x + 1)
            ((\x -> x) x))



-- | LESSON 4

ifEvenInc n = if even n
              then n + 1
              else n

ifEvenDouble n = if even n
                 then n * 2
                 else n

ifEvenSquare n = if even n
                 then n ^ 2
                 else n

ifEven myFunction x = if even x
                      then myFunction x
                      else x


ifEvenInc' n = ifEven inc n
ifEvenDouble' n = ifEven double n
ifEvenSquare' n = ifEven square n

ifEvenCube = ifEven (\x -> x ^ 3)


author = ("Will", "Kurt")

names = [ ("Ian", "Curtis")
        , ("Bernard","Sumner")
        , ("Peter", "Hook")
        , ("Stephen","Morris")
        ]

compareLastNames name1 name2 =
        if lastName1 > lastName2
        then GT
        else if lastName1 < lastName2
            then LT
            else EQ
    where lastName1 = snd name1
          lastName2 = snd name2

compareLastNames' name1 name2 =
        if lastName1 > lastName2
        then GT
        else if lastName1 < lastName2
            then LT
            else if firstName1 < firstName2
                 then LT
                 else EQ
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2


addressLetter name location = nameText ++ " - " ++ location
    where nameText = fst name ++ " " ++ snd name

sfOffice name = if lastName < "L"
                then nameText
                    ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                    ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = fst name ++ " " ++ snd name

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = fst name ++ " " ++ snd name

getLocationFunction location = case location of
    "ny"   -> nyOffice
    "sy"   -> sfOffice
    "reno" -> renoOffice
    _      -> \name -> fst name ++ " " ++ snd name

addressLetter' name location = locationFunction name
    where locationFunction = getLocationFunction location


-- Q4.1 Anything that can be compared in Haskell (for
-- example, [Char], which you use for the names in your
-- name tuples) can be compared with a function called
-- compare. The compare function returns GT, LT, or EQ.
-- Rewrite compareLastNames by using compare.

compareLastNames'' name1 name2 = compare lastName1 lastName2
    where lastName1 = snd name1
          lastName2 = snd name2


-- Q4.2 Define a new location function for Washington, DC
-- and add it to getLocationFunction. In the DC function,
-- everyone’s names must be followed by Esq.

dcOffice name = nameText ++ " - PO Box 666 - Washington, DC 111"
    where nameText = fst name ++ " " ++ snd name ++ ", Esq."

getLocationFunction' location = case location of
    "ny"   -> nyOffice
    "sy"   -> sfOffice
    "reno" -> renoOffice
    "dc"   -> dcOffice
    _      -> \name -> fst name ++ " " ++ snd name



-- | LESSON 5

genIfEven f = (\x -> ifEven f x)

genIfXEven x = (\f -> ifEven f x)


getRequestURL host apiKey resource id =
    host ++ "/" ++ resource ++ "/" ++ id ++
    "?token=" ++ apiKey

genHostRequestBuilder host =
    (\apiKey resource id ->
        getRequestURL host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey =
    (\resource id ->
        hostBuilder apiKey resource id)

myExampleUrlBuilder =
    genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

genApiRequestBuilder' hostBuilder apiKey resource =
    (\id ->
        hostBuilder apiKey resource id)


add4 a b c d = a + b + c + d

addXto3 x = (\b c d ->
    add4 x b c d)

addXto2 x y = (\c d ->
    add4 x y c d)


exampleUrlBuilder' = getRequestURL "http://example.com"

myExampleUrlBuilder' = exampleUrlBuilder' "1337hAsk3ll"

exampleBuilder = myExampleUrlBuilder' "books"


addressLetterV2 location name = addressLetter name location

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

addressLetterV2' = flipBinaryArgs addressLetter
addressLetterNY = addressLetterV2' "ny"


subtract2 = flip (-) 2


-- Q5.1 Now that you know about partial application, you no
-- longer need to use genIfEvenX. Redefine ifEvenInc,
-- ifEvenDouble, and ifEvenSquare by using ifEven and
-- partial application.

ifEvenInc'' = ifEven inc
ifEvenDouble'' = ifEven double
ifEvenSquare'' = ifEven square


-- Q5.2 Even if Haskell didn’t have partial application,
-- you could hack together some approximations. Following a
-- similar pattern to flipBinaryArgs (figure 5.6), write a
-- function binaryPartialApplication that takes a binary
-- function and one argument and returns a new function
-- waiting for the missing argument.

binaryPartialApplication binaryFunction x =
    (\y -> binaryFunction x y)



-- | LESSON 6

isPalindrome word = word == reverse word

respond phrase = if '!' `elem` phrase
                 then "wow!"
                 else "uh.. okay"

takeLast n aList = reverse (take n (reverse aList))

one n = take n (cycle [1])

assignToGroups n aList = zip groups aList
    where groups = cycle [1 .. n]


-- Q6.1 Haskell has a function called repeat that takes a
-- value and repeats it infinitely. Using the functions
-- you’ve learned so far, implement your own version
-- of repeat
repeat' n = cycle [n]


-- Q6.2 Write a function subseq that takes three arguments:
-- a start position, an end position, and a list.
-- The function should return the subsequence between the
-- start and end. For example:
--  GHCi> subseq 2 5 [1 .. 10]
--  [3,4,5]
--  GHCi> subseq 2 7 "a puppy"
--  "puppy"
subseq start end aList = take (end - start) $ drop start aList


-- Q6.3 Write a function inFirstHalf that returns True if
-- an element is in the first half of a list, and  otherwise
-- returns False
inFirstHalf el aList = el `elem` halfList
    where halfList = take (length aList `div` 2) aList



-- | LESSON 7

myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
    where remainder = a `mod` b

-- sayAmount n = case n of
--     1 -> "one"
--     2 -> "two"
--     n -> "a bunch"
sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"

isEmpty [] = True
isEmpty _  = False

myHead (x:_) = x
myHead []    = error "No head for empty list"


-- Q7.1 The tail function in Haskell returns an error when
-- called on an empty list. Modify myTail so that it does
-- handle the case of an empty list by returning the empty
-- list.
myTail []     = []
myTail (_:xs) = xs


-- Q7.2 Rewrite myGCD by using pattern matching.

-- myGCD' b 0         = b
-- myGCD' b remainder = myGCD' b remainder

myGCD' a 0 = a
myGCD' a b = myGCD' b (a `mod` b)



-- | LESSON 8

myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _  = []
myTake n (x:xs) = x : rest
    where rest = myTake (n - 1) xs

finiteCycle (first:rest) = first:rest ++ [first]

myCycle (first:rest) = first:myCycle (rest ++ [first])


ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))

collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n * 3 + 1)


-- Q8.1 Implement your own version of reverse, which
-- reverses a list.
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x]


-- Q8.2 Calculating Fibonacci numbers is perhaps the
-- single most common example of a recursive function. The
-- most straightforward definition is as follows:
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Like the Ackermann function, this implementation quickly
-- explodes due to the mutually recursive calls. But unlike
-- the Ackermann function, there’s a much more efficient way
-- to compute the nth Fibonacci number. Write a function,
-- fastFib, that can compute the 1,000th Fibonacci number
-- nearly instantly.
-- Hint: fastFib takes three arguments: n1, n2, and counter.
-- To calculate the 1,000th Fibonacci number, you call
-- fastFib 1 1 1000 and for the 5th, you call fastFib 1 1 5.
fastFib _ _ 0         = 0
fastFib _ _ 1         = 1
fastFib _ _ 2         = 1
fastFib n1 n2 3       = n1 + n2
fastFib n1 n2 counter = fastFib (n1 + n2) n1 (counter - 1)
fib' n = fastFib 1 1 n



-- | LESSON 9

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
