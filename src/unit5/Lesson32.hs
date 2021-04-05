import           Control.Monad (guard)
import           Data.Char     (toUpper)


powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return $ 2 ^ value

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (^ 2) [1 .. n]

powersOfTwoAndTree :: Int -> [(Int, Int)]
powersOfTwoAndTree n = do
    value <- [1 .. n]
    let powersOfTwo = 2 ^ value
    let powersOfThree = 3 ^ value
    return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenValue <- [2, 4 .. n]
    oddValue <- [1, 3 .. n]
    return (evenValue, oddValue)

numbersAndSquares10 :: [(Int, Int)]
numbersAndSquares10 = do
    value <- [1 .. 10]
    let square = value ^ 2
    return (value, square)

evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1 .. n]
    guard $ even value
    return value

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f v = do
    val <- v
    guard $ f val
    return val


evenSquares :: [Int]
evenSquares = do
    n <- [0 .. 9]
    let nSquared = n ^ 2
    guard $ even nSquared
    return nSquared

powersOfTwo' :: Int -> [Int]
powersOfTwo' n = [ value ^ 2 | value <- [1 .. n] ]

powersOfTwoAndThree' :: Int -> [(Int, Int)]
powersOfTwoAndThree' n = [ (powersOfTwo, powersOfThree)
                         | value <- [1 .. n]
                         , let powersOfTwo = 2 ^ value
                         , let powersOfThree = 3 ^ value
                         ]

allEvenOdds' :: Int -> [(Int, Int)]
allEvenOdds' n = [ (evenValue, oddValue)
                 | evenValue <- [2, 4 .. n]
                 , oddValue <- [1, 3 .. n]
                 ]

evensGuard' :: Int -> [Int]
evensGuard' n = [ value | value <- [1 .. n], even value ]

mrColor :: [String]
mrColor = [ "Mr. " ++ result
          | colors <- ["brown", "blue", "pink", "orange"]
          , let result = (\(x:xs) -> toUpper x : xs) colors
          ]


-- Q32.1 Use a list comprehension that generates a list of
-- correct calendar dates, given that you know the number
-- of days in each month. For example, it should start with
-- 1 .. 31 for January and be followed by 1 .. 28
-- for February.
daysInMonth :: [Int]
daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

calendarDates :: Int -> [Int]
calendarDates month = [ days | days <- [1 .. daysInMonth !! month + 1] ]


-- Q32.2 Translate the preceding question into do-notation,
-- and then into Monad methods and lambdas.
calendarDatesDo :: Int -> [Int]
calendarDatesDo month = do
    days <- [1 .. daysInMonth !! month + 1]
    return days

calendarDatesMonad :: Int -> [Int]
calendarDatesMonad month = return month >>= (\month -> [1 .. daysInMonth !! month + 1])
