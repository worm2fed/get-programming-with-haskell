aList :: [String]
aList = ["cat", "dog", "mouse"]


addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2


class TypeName a where
    fun1 :: a -> a
    fun2 :: a -> String
    fun3 :: a -> a -> Bool

class Describable a where
    describe :: a -> String


data Icecream = Chocolate | Vanilla
    deriving (Show, Eq, Ord)


-- Q13.1 If you ran the :info examples, you likely noticed
-- that the type Word has come up a few times. Without
-- looking at external resources, use :info to explore Word
-- and the relevant type classes to come up with your own
-- explanation for the Word type.
-- How is it different from Int?
-- minBound :: Word => 0


-- Q13.2 One type class we didn’t discuss is Enum. Use :info
-- to look at the definition of this type class, as well as
-- example members. Now consider Int, which is an instance
-- of both Enum and Bounded. Given the following definition of inc:
inc :: Int -> Int
inc x = x + 1
-- and the succ function required by Enum, what’s the
-- difference between inc and succ for Int?
-- succ (maxBound :: Int) =>
--      *** Exception: Prelude.Enum.succ{Int}: tried to
--      take `succ' of maxBound
-- inc (maxBound :: Int) => -9223372036854775808


-- Q13.3 Write the following function that works just like
-- succ on Bounded types but can be called an unlimited number
-- of times without error. The function will work like inc in
-- the preceding example but works on a wider range of types, including types that aren’t members of Num
-- Your definition will include functions/values from
-- Bounded, Enum, and the mystery type class. Make a note
-- of where each of these three (or more) functions/values
-- comes from.
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n