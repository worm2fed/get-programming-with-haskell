{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

import qualified Data.Text.Lazy    as L
import qualified Data.Text.Lazy.IO as LIO


firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- throws error
myWord :: T.Text
myWord = "dog"

myNum1 :: Int
myNum1 = 3

myNum2 :: Integer
myNum2 = 3

myNum3 :: Double
myNum3 = 3

-- ghc template.hs -XTemplateHaskell
-- {-# LANGUAGE TemplateHaskell #-}

sampleInput :: T.Text
sampleInput = "this\nis\ninput"
-- T.lines sampleInput
-- T.unlines (T.lines sampleInput)

someText :: T.Text
someText = "Some\ntext for\t you"
-- T.words someText
-- T.unwords (T.words someText)

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"
-- T.splitOn breakText exampleText
-- T.intercalate breakText (T.splitOn breakText exampleText)

combined :: String
combined = "some" ++ " " ++ "strings"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

myTextLines :: T.Text -> [T.Text]
myTextLines = T.splitOn "\n"

myTextUnLines :: [T.Text] -> T.Text
myTextUnLines = T.intercalate "\n"


-- Q23.1 Rewrite the hello_world.hs program (reproduced
-- here) from lesson 21 to use Text instead of String types.
helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

mainHelloPerson :: IO ()
mainHelloPerson = do
    TIO.putStrLn "Hello! What's your name?"
    name <- TIO.getLine
    let statement = helloPerson name
    TIO.putStrLn statement


-- Q23.2 Use Data.Text.Lazy and Data.Text.Lazy.IO to
-- rewrite the lazy I/O section from lesson 22 by using
-- the Text type.
toInts :: L.Text -> [Int]
toInts = map (read . L.unpack) . L.lines

mainSumInts :: IO ()
mainSumInts = do
   userInput <- LIO.getContents
   let numbers = toInts userInput
   print (sum numbers)
