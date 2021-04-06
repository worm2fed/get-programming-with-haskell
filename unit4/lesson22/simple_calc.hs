-- Q22.1 Write a program, simple_calc.hs, that reads simple
-- equations involving adding two numbers or multiplying
-- two numbers. The program should solve the equation each
-- user types into each line as each line is entered.

evaluate :: String -> String -> String -> Int
evaluate x "+" y = read x + read y
evaluate x "*" y = read x * read y


main :: IO ()
main = do
    userInput <- getContents
    let equations = map words $ lines userInput
    mapM_ (print . (\[x, op, y] -> evaluate x op y)) equations
