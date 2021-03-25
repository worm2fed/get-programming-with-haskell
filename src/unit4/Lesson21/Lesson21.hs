import qualified Data.Map as Map
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

-- name <- getLine :: IO String

-- no, getLine has type IO String, and main has IO ()

-- no, cause type mismatch

-- Q21.1 Translate helloPerson and main into code by using
-- do-notation in a Maybe. Assume that all the user input
-- is replaced with a Map with a value for the input.
-- Ignore the first putStrLn and simply return the statement
-- at the end.
userInput :: Map.Map Int String
userInput = Map.fromList [(1, "Bob"), (2, "Damian")]

mainMaybe :: Maybe String
mainMaybe = do
    name <- Map.lookup 1 userInput
    let statement = helloPerson name
    return statement

-- Q21.2 Create a program that asks the user to input a
-- number and then returns the nth Fibonacci numbers
-- (see lesson 8 for an example of computing Fibonacci
-- numbers).
fastFib :: Integer -> Integer -> Integer -> Integer
fastFib _ _ 0   = 0
fastFib _ _ 1   = 1
fastFib _ _ 2   = 1
fastFib n1 n2 3 = n1 + n2
fastFib n1 n2 c = fastFib (n1 + n2) n1 (c - 1)

fib :: Integer -> Integer
fib = fastFib 1 1

mainFib :: IO ()
mainFib = do
    putStrLn "Enter n for fib function:"
    n <- getLine
    let result = fib (read n)
    putStrLn (show result)
