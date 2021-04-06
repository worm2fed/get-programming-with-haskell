import qualified Data.Map as Map


type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [ (1, "nYarlathoTep")
                          , (2, "KINGinYELLOW")
                          , (3, "dagon1997")
                          , (4, "rcarter1919")
                          , (5, "xCTHULHUx")
                          , (6, "yogSOThoth")
                          ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [ ("nYarlathoTep", 2000)
                         , ("KINGinYELLOW", 15000)
                         , ("dagon1997", 300)
                         , ("rcarter1919", 12)
                         , ("xCTHULHUx", 50000)
                         , ("yogSOThoth", 150000)
                         ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing         = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromId' :: GamerId -> Maybe PlayerCredits
creditsFromId' id = altLookupCredits (lookupUserName id)

-- creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)
-- creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id
-- it has Maybe inside Maybe

-- we can't use pattern matching for IO

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

type WllCoId = Int

gamerIdDB :: Map.Map WllCoId GamerId
gamerIdDB = Map.fromList [ (1001, 1)
                         , (1002, 2)
                         , (1003, 3)
                         , (1004, 4)
                         , (1005, 5)
                         , (1006, 6)
                         ]

lookupGamerId :: WllCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WllCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits


readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print $ n * 2

readIntAndPrintDouble :: IO ()
readIntAndPrintDouble = readInt >>= printDouble


askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

productTwo :: Num a => a -> IO a
productTwo = (\a -> return $ (+2) a)

helloName :: IO ()
helloName = askForName
         >> getLine
        >>= (\name -> return $ nameStatement name)
        >>= putStrLn


-- Q30.1 To prove that Monad is strictly more powerful than
-- Functor, write a universal ver- sion of <$>, as in the
-- preceding lesson’s exercise, called allFmapM, that
-- defines <$> for all members of the Monad type class.
-- Because it works for all instances of Monad, the only
-- functions you can use are the methods required by the
-- Monad type class (and lambda functions).
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f m = m >>= \x -> return $ f x


-- Q30.2 To prove that Monad is strictly more powerful than
-- Applicative, write a universal version of <*>, called
-- allApp, that defines <*> for all members of the Monad
-- type class. Because it works for all instances of Monad,
-- the only functions you can use are the meth- ods required
-- by the Monad type class (and lambda functions).
-- This question is much trickier than the last one.
-- Two hints:
--      - Try to think exclusively in terms of the type
--          signatures.
--      - Use <$> if you want and replace it with your
--          answer to Q29.1
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mF m = mF >>= (\f -> m >>= (\x -> return $ f x))


-- Q30.3 Implement a bind function which is the same as
-- (>>=) for Maybe:
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _  = Nothing
bind (Just x) f = f x
