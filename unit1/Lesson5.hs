ifEven myFunction x = if even x
                      then myFunction x
                      else x

enIfEven f = (\x -> ifEven f x)

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


addressLetter name location = nameText ++ " - " ++ location
    where nameText = fst name ++ " " ++ snd name
addressLetterV2 location name = addressLetter name location

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

addressLetterV2' = flipBinaryArgs addressLetter
addressLetterNY = addressLetterV2' "ny"


subtract2 = flip (-) 2


-- Q5.1 Now that you know about partial application, you no
-- longer need to use genIfEvenX. Redefine ifEvenInc,
-- ifEvenDouble, and ifEvenSquare by using ifEven and
-- partial application.
-- ifEvenInc'' = ifEven inc
-- ifEvenDouble'' = ifEven double
-- ifEvenSquare'' = ifEven square


-- Q5.2 Even if Haskell didn’t have partial application,
-- you could hack together some approximations. Following a
-- similar pattern to flipBinaryArgs (figure 5.6), write a
-- function binaryPartialApplication that takes a binary
-- function and one argument and returns a new function
-- waiting for the missing argument.

binaryPartialApplication binaryFunction x =
    (\y -> binaryFunction x y)
