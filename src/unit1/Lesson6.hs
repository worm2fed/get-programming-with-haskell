isPalindrome word = word == reverse word

respond phrase = if '!' `elem` phrase
                 then "wow!"
                 else "uh.. okay"

takeLast n aList = reverse (take n (reverse aList))

one n = take n (cycle [1])

assignToGroups n aList = zip groups aList
    where groups = cycle [1 .. n]


-- Q6.1 Haskell has a function called repeat that takes a
-- value and repeats it infinitely. Using the functions
-- you’ve learned so far, implement your own version
-- of repeat
repeat' n = cycle [n]


-- Q6.2 Write a function subseq that takes three arguments:
-- a start position, an end position, and a list.
-- The function should return the subsequence between the
-- start and end. For example:
--  GHCi> subseq 2 5 [1 .. 10]
--  [3,4,5]
--  GHCi> subseq 2 7 "a puppy"
--  "puppy"
subseq start end aList = take (end - start) $ drop start aList


-- Q6.3 Write a function inFirstHalf that returns True if
-- an element is in the first half of a list, and  otherwise
-- returns False
inFirstHalf el aList = el `elem` halfList
    where halfList = take (length aList `div` 2) aList
