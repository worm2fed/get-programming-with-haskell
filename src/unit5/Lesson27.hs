import qualified Data.Map           as Map
import           System.Environment


successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just n) = Just $ n + 1
incMaybe Nothing  = Nothing

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe (Just s) = Just $ reverse s
reverseMaybe Nothing  = Nothing


-- instance Functor Maybe where
--     fmap f (Just n) = Just $ f n
--     fmap _ Nothing  = Nothing

successStr :: Maybe String
successStr = show <$> successfulRequest

failStr :: Maybe String
failStr = show <$> failedRequest

reverseStr :: Maybe String -> Maybe String
reverseStr a = reverse <$> a


data RobotPart = RobotPart
    { name        :: String
    , description :: String
    , cost        :: Double
    , count       :: Int
    } deriving Show

leftArm :: RobotPart
leftArm  = RobotPart
    { name = "left arm"
    , description = "left arm for face punching!"
    , cost = 1000.00
    , count = 3
    }

rightArm :: RobotPart
rightArm  = RobotPart
    { name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.00
    , count = 5
    }

robotHead :: RobotPart
robotHead  = RobotPart
    { name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [ "<h2>", partName, "</h2>"
                          , "<p><h3>desc</h3>", partDesc, "</p>"
                          , "<p><h3>cost</h3>", partCost, "</p>"
                          , "<p><h3>count</h3>", partCount, "</p>"
                          ]
    where partName = name part
          partDesc = description part
          partCost = show $ cost part
          partCount = show $ count part

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyValues
    where keys = [1, 2, 3]
          values = [leftArm, rightArm, robotHead]
          keyValues = zip keys values

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO


-- Q27.1 When we introduced parameterized types in
-- lesson 15, you used a minimal type Box as the example:
data Box a = Box a
    deriving Show
-- Implement the Functor type class for Box. Then implement
-- morePresents, which changes a box from type Box a to one
-- of type Box [a], which has n copies of the original
-- value in the box in a list. Make sure to use fmap to
-- implement this.
instance Functor Box where
    fmap f (Box x) = Box $ f x

morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap $ replicate n


-- QC27.2 Now suppose you have a simple box like this:
myBox :: Box Int
myBox = Box 1
-- Use fmap to put the value in your Box in another Box.
-- Then define a function unwrap that takes a value out of
-- a box, and use fmap on that function to get your original
-- box. Here’s how your code should work in GHCi:
-- GHCi> wrapped = fmap ? myBox
-- GHCi> wrapped
-- Box (Box 1)
-- GHCi> fmap unwrap wrapped
-- Box 1
wrapped :: Box (Box Int)
wrapped = fmap Box myBox

unwrap :: Box a -> a
unwrap (Box x) = x


-- Q27.3 Write a command-line interface for partsDB that
-- lets the user look up the cost of an item, given an ID.
-- Use the Maybe type to handle the case of the user
-- entering missing input.
getCost :: IO (Maybe Double)
getCost = do
    args <- getArgs
    let itemId = read $ head args :: Int
    let item = Map.lookup itemId partsDB
    return $ cost <$> item
