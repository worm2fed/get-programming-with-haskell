import           Data.List      (sort)
import           Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = foldr (&&) True . map testFunc

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = foldr (||) False . map testFunc


instance Semigroup Integer where
    (<>) x y = x + y


data Color = Red
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange
           | Brown
           | White
    deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red    Blue    = Purple
    (<>) Blue   Red     = Purple
    (<>) Yellow Blue    = Green
    (<>) Blue   Yellow  = Green
    (<>) Yellow Red     = Orange
    (<>) Red    Yellow  = Orange

    (<>) White b     = b
    (<>) a     White = a

    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple])   [a, b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | otherwise = Brown

howMuch :: Int -> String
howMuch n | n > 10    = "a whole bunch"
          | n > 0     = "not much"
          | otherwise = "we're in debt!"


-- type Events = [String]
-- type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events (Probs probs) = PTable events normalizedProbs
    where totalProbs = sum probs
          normalizedProbs = Probs (map (\x -> x/ totalProbs) probs)

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show (PTable (Events events) (Probs probs)) = mconcat pairs
        where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where combiner = \x y -> mconcat [x, "-", y]

combineProbs:: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup PTable where
    (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
    (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2

    (<>) (PTable e1 p1) (PTable e2 p2) =
            createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs  = combineProbs  p1 p2

instance Monoid PTable where
    mempty = PTable (Events []) (Probs [])

coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])


-- Q17.1 Your current implementation of Color doesn’t
-- contain an identity element. Mod- ify the code in this
-- unit so that Color does have an identity element, and
-- then make Color an instance of Monoid.
instance Monoid Color where
    mempty = White

-- Q17.2 If your Events and Probs types were data types and
-- not just synonyms, you could make them instances of
-- Semigroup and Monoid, where combineEvents and
-- combineProbs were the <> operator in each case. Refactor
-- these types and make instances of Semigroup and Monoid.
newtype Events = Events [String]
instance Semigroup Events where
    (<>) = combineEvents
instance Monoid Events where
    mempty = Events []

newtype Probs = Probs [Double]
instance Semigroup Probs where
    (<>) = combineProbs
instance Monoid Probs where
    mempty = Probs []
