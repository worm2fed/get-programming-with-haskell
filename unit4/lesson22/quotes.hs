-- Q22.2 Write a program that allows a user to select a
-- number between 1 and 5 and then prints a famous quote
-- (quotes are of your choosing). After printing the quote,
-- the program will ask whether the user would like another.
-- If the user enters n, the program ends; otherwise, the
-- user gets another quote. The program repeats until the
-- user enters n. Try to use lazy evaluation and treat the
-- user input as a list rather than recursively calling
-- main at the end.

quotes :: [String]
quotes = [ "first"
         , "second"
         , "third"
         , "fourth"
         , "fifth"
         ]

getQuote :: [String] -> [String]
getQuote []      = []
getQuote ("n":_) = []
getQuote (x:xs)  = quote : getQuote xs
    where quote = quotes !! (read x - 1)

main :: IO ()
main = do
    userInput <- getContents
    mapM_ putStrLn . getQuote . lines $ userInput



