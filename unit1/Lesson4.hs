ifEvenInc n = if even n
              then n + 1
              else n

ifEvenDouble n = if even n
                 then n * 2
                 else n

ifEvenSquare n = if even n
                 then n ^ 2
                 else n

ifEven myFunction x = if even x
                      then myFunction x
                      else x


inc x = x + 1
double x = x * 2
square x = x * x

ifEvenInc' n = ifEven inc n
ifEvenDouble' n = ifEven double n
ifEvenSquare' n = ifEven square n

ifEvenCube = ifEven (\x -> x ^ 3)


author = ("Will", "Kurt")

names = [ ("Ian", "Curtis")
        , ("Bernard","Sumner")
        , ("Peter", "Hook")
        , ("Stephen","Morris")
        ]

compareLastNames name1 name2 =
        if lastName1 > lastName2
        then GT
        else if lastName1 < lastName2
            then LT
            else EQ
    where lastName1 = snd name1
          lastName2 = snd name2

compareLastNames' name1 name2 =
        if lastName1 > lastName2
        then GT
        else if lastName1 < lastName2
            then LT
            else if firstName1 < firstName2
                 then LT
                 else EQ
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2


addressLetter name location = nameText ++ " - " ++ location
    where nameText = fst name ++ " " ++ snd name

sfOffice name = if lastName < "L"
                then nameText
                    ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                    ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = fst name ++ " " ++ snd name

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = fst name ++ " " ++ snd name

getLocationFunction location = case location of
    "ny"   -> nyOffice
    "sy"   -> sfOffice
    "reno" -> renoOffice
    _      -> \name -> fst name ++ " " ++ snd name

addressLetter' name location = locationFunction name
    where locationFunction = getLocationFunction location


-- Q4.1 Anything that can be compared in Haskell (for
-- example, [Char], which you use for the names in your
-- name tuples) can be compared with a function called
-- compare. The compare function returns GT, LT, or EQ.
-- Rewrite compareLastNames by using compare.
compareLastNames'' name1 name2 = compare lastName1 lastName2
    where lastName1 = snd name1
          lastName2 = snd name2


-- Q4.2 Define a new location function for Washington, DC
-- and add it to getLocationFunction. In the DC function,
-- everyone’s names must be followed by Esq.
dcOffice name = nameText ++ " - PO Box 666 - Washington, DC 111"
    where nameText = fst name ++ " " ++ snd name ++ ", Esq."

getLocationFunction' location = case location of
    "ny"   -> nyOffice
    "sy"   -> sfOffice
    "reno" -> renoOffice
    "dc"   -> dcOffice
    _      -> \name -> fst name ++ " " ++ snd name
