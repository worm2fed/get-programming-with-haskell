echo :: IO ()
echo = getLine >>= putStrLn

echoVerbose :: IO ()
echoVerbose =
    putStrLn "Enter a String and we'll echo it!" >> echo

main :: IO ()
main = echoVerbose
