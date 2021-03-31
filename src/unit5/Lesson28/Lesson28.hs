import qualified Data.Map as Map


-- haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
-- haversineMaybe Nothing _               = Nothing
-- haversineMaybe _ Nothing               = Nothing
-- haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe Nothing b         = b
addMaybe a Nothing         = a
addMaybe (Just a) (Just b) = Just $ a + b

-- distanceFromNY = haversine newYork

val1 = Just 10
val2 = Just 5

myMultiply = (+) <$> val1 <*> val2
myDiv = div <$> val1 <*> val2
myMod = mod <$> val1 <*> val2


-- minOfThree <$> Just 10 <*> Just 3 <*> Just 6


data User = User
    { username :: String
    , gamerId  :: Int
    , score    :: Int
    } deriving Show


serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001


readInt :: IO Int
readInt = read <$> getLine

mainUser :: IO ()
mainUser = do
    putStrLn "Enter a username, gamerId and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user


-- User <$> Nothing <*> serverGamerId <*> serverScore ==> Nothing


-- Q28.1 Writing haversineMaybe (listing 28.4) was
-- straightforward. Write the function haversineIO without
-- using <*>.
-- haversineIO :: IO LatLong -> IO LatLong -> IO Double
-- haversineIO coords1 coords2 = do
--     a <- coords1
--     b <- coords2
--     return $ haversine a b


-- Q28.2 Rewrite haversineIO, this time using <*>.
-- haversineIO :: IO LatLong -> IO LatLong -> IO Double
-- haversineIO a b = haversine <$> a <*> b


-- Q28.3 Recall the RobotPart type from the preceding
-- lesson:
data RobotPart = RobotPart
    { name        :: String
    , description :: String
    , cost        :: Double
    , count       :: Int
    } deriving Show
-- Make a command-line application that has a database of
-- various RobotParts (at least five), and then lets the
-- user enter in two-part IDs and returns the one with the
-- lowest cost. Handle the case of the user entering an ID
-- that’s not in the parts database.
leftLag :: RobotPart
leftLag  = RobotPart
    { name = "left lag"
    , description = "left lag for fmoving"
    , cost = 1000.00
    , count = 3
    }

rightLag :: RobotPart
rightLag  = RobotPart
    { name = "right lag"
    , description = "right lag for moving"
    , cost = 1025.00
    , count = 5
    }

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

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyValues
    where keys = [1, 2, 3, 4, 5]
          values = [leftLag, rightLag, leftArm, rightArm, robotHead]
          keyValues = zip keys values

getMinCost :: IO (Maybe Double)
getMinCost = do
    putStrLn "Enter first id"
    id1 <- readInt
    putStrLn "Enter second id"
    id2 <- readInt
    let itemCost1 = cost <$> Map.lookup id1 partsDB
    let itemCost2 = cost <$> Map.lookup id2 partsDB
    return $ min <$> itemCost1 <*> itemCost2
