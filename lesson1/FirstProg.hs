messyMain :: IO ()
messyMain = do
    print "Who is the email for?"
    recipient <- getLine
    print "What is the Title?"
    title <- getLine
    print "Who is the Author?"
    author <- getLine
    print ("Dear " ++ recipient ++ ",\n" ++
            "Thanks for buying " ++ title ++
            "\nthanks,\n" ++ author
          )

toPart recipient =
    "Dear " ++ recipient ++ ",\n"
bodyPart bookTitle =
    "Thanks for buying " ++ bookTitle ++ ".\n"
fromPart author =
    "Thanks, \n" ++ author

createEmail recipient bookTitle author =
    toPart recipient ++
    bodyPart bookTitle ++
    fromPart author

main = do
    print "Specify who is the email for?"
    recipient <- getLine
    print "Specify what is the Title?"
    title <- getLine
    print "Specify who is the Author?"
    author <- getLine
    print (createEmail recipient title author)

