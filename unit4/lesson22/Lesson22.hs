import           Data.List.Split
import           System.Environment


main :: IO ()
main = do
    args <- getArgs
    mapM_ putStrLn args

main' :: IO ()
main' = do
    lines <- mapM (\_ -> getLine) [1 .. 3]
    mapM_ putStrLn lines


myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n action = mapM (\_ -> action) [1 .. n]


main'' :: IO ()
main'' = do
    userInput <- getContents
    mapM_ print userInput

reverseInput :: IO ()
reverseInput = do
    userInput <- getContents
    (putStrLn . reverse) userInput

sampleData = ['6', '2', '\n', '2', '1', '\n']

myLines = splitOn "\n"

toInts :: String -> [Int]
toInts = map read . lines


sumSquares :: IO ()
sumSquares = do
    userInput <- getContents
    let numbers = toInts userInput
    print . sum . map (^ 2) $ numbers
