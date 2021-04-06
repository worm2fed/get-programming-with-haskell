combineTwoStrings :: Maybe String -> Maybe String -> Maybe String
combineTwoStrings s1 s2 = (++) <$> s1 <*> s2


helloIO :: IO String
helloIO = pure "Hello World"


newtype Box a = Box a

data ResourceConstrained a = NoResources | Okay a

-- because the result type will be different


doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

-- totalPrize = pure (*) <*> doorPrize <*> multiplier


primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where twoThroughN = [2 .. n]
          composite = pure (*) <*> twoThroughN <*> twoThroughN
          isNotComposite = not . (`elem` composite)


data User = User
    { name    :: String
    , gamerId :: Int
    , score   :: Int
    } deriving Show

testNames :: [String ]
testNames = [ "John Smith"
            , "Robert'); DROP TABLE Students;--"
            , "Christina NULL"
            , "Randall Munroe"
            , "worm2fed"
            ]

testIds :: [Int]
testIds = [ 1337
          , 0123
          , 999999
          ]

testScores :: [Int]
testScores = [ 0
             , 100000
             , -99999
             ]

testData :: [User]
testData = pure User <*> testNames
                     <*> testIds
                     <*> testScores

-- after adding my name there are will be 45 test users


-- Q29.1 To prove that Applicative is strictly more powerful
-- than Functor, write a universal version of fmap, called
-- allFmap, that defines fmap for all members of the
-- Applicative type class. Because it works for all
-- instances of Applicative, the only functions you can use
-- are the methods required by the Applicative type class.
-- To get you started, here’s your type signature:
-- allFmap :: Applicative f => (a -> b) -> f a -> f b
-- When you’re finished, test this out on List and Maybe,
-- which are both members of Applicative:
--  GHCi> allFmap (+ 1) [1,2,3]
--  [2,3,4]
--  GHCi> allFmap (+ 1) (Just 5)
--  Just 6
--  GHCi> allFmap (+ 1) Nothing
--  Nothing
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f v = pure f <*> v


-- Q29.2 Translate the following expression into one where
-- the result is a Maybe Int. The catch is that you may not
-- add (or remove) anything to the code except pure and <*>.
-- You can’t use the Just constructor or any extra parentheses.
example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure $ (*) ((+) 2 4) 6


-- Q29.3 Take the following example and use nondeterministic
-- computing with Lists to determine how much beer you need
-- to purchase to assure there will be enough:
--      - You bought beer last night but don’t remember
--          whether it was a 6-pack or a 12- pack.
--      - You and your roommate each had two beers last
--          night.
--      - You’re having either two or three friends coming
--          over tonight, depending on who can come.
--      - For a long night of gaming, you expect the average
--          person to drink three to four beers.
boughtLastNight :: [Int]
boughtLastNight = [6, 12]

remains :: [Int]
remains = (\count -> count - 4) <$> boughtLastNight

friendsComing :: [Int]
friendsComing = [2, 3]

totalDrinkers :: [Int]
totalDrinkers = (+ 2) <$> friendsComing

expectedDrinking  :: [Int]
expectedDrinking = [3, 4]

needBeer :: [Int]
needBeer = pure (*) <*> expectedDrinking <*> totalDrinkers

needToBuy = pure (-) <*> needBeer <*> remains
