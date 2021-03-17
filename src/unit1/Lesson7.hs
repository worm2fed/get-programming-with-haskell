myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
    where remainder = a `mod` b

-- sayAmount n = case n of
--     1 -> "one"
--     2 -> "two"
--     n -> "a bunch"
sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"

isEmpty [] = True
isEmpty _  = False

myHead (x:_) = x
myHead []    = error "No head for empty list"


-- Q7.1 The tail function in Haskell returns an error when
-- called on an empty list. Modify myTail so that it does
-- handle the case of an empty list by returning the empty
-- list.
myTail []     = []
myTail (_:xs) = xs


-- Q7.2 Rewrite myGCD by using pattern matching.
-- myGCD' b 0         = b
-- myGCD' b remainder = myGCD' b remainder

myGCD' a 0 = a
myGCD' a b = myGCD' b (a `mod` b)
