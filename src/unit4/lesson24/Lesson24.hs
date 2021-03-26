import qualified Data.Text.IO       as TI
import           System.Environment


-- openFile "stuff.txt" ReadMode

-- unwords better then ++ because ++ works only with lists


-- Q24.1 Write a version of the Unix cp program that will
-- copy a file and allow you to rename it (just mimic the
-- basic functionality and don’t worry about specific flags).
cp :: IO ()
cp = do
    args <- getArgs
    let (from:to:_) = args
    content <- TI.readFile from
    TI.writeFile to content
