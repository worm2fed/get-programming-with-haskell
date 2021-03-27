{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import qualified Data.Text.IO          as TIO
import           System.Environment
import           System.Random


sampleBytes ::B.ByteString
sampleBytes = "Hello"

sampleString :: String
-- sampleString = B.unpack sampleBytes <== error
sampleString = BC.unpack sampleBytes

bcInt :: BC.ByteString
bcInt = "6"

convertBC2Int :: BC.ByteString -> Int
convertBC2Int = read . BC.unpack


getRandomChar :: IO Char
getRandomChar = do
    randomNumber <- randomRIO (0, 255)
    return $ toEnum randomNumber


nagarjunaBC :: BC.ByteString
nagarjunaBC = "नागर्जुनॅ"

nagarjunaText :: T.Text
nagarjunaText = "नागर्जुनॅ"

nagarjunaB :: B.ByteString
nagarjunaB = (BC.pack . T.unpack) nagarjunaText

nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText


-- Q25.1 Write a program that reads in a text file and
-- outputs the difference between the number of characters
-- in the file and the number of bytes in the file.
charsAndBytesDiff :: IO ()
charsAndBytesDiff = do
    args <- getArgs
    let fileName = head args
    file <- B.readFile fileName
    putStrLn "Number of bytes: "
    print $ B.length file
    putStrLn "Number of chars: "
    print $ T.length . E.decodeUtf8 $ file
