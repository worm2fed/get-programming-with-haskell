import           System.IO


main :: IO ()
main = do
    helloFile <- openFile "hello.txt" ReadMode
    hasFirstLine <- hIsEOF helloFile
    firstLine <- if not hasFirstLine
                 then hGetLine helloFile
                 else return "empty"
    putStrLn firstLine
    hasSecondLine <- hIsEOF helloFile
    secondLine <- if not hasSecondLine
                 then hGetLine helloFile
                 else return ""
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile secondLine
    hClose helloFile
    hClose goodbyeFile
    putStrLn "done!"

