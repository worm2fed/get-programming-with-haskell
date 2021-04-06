module Main where

import           Control.Monad         (foldM)
import qualified Data.ByteString.Char8 as BC
import           System.Environment    (getArgs)
import           Glitch (glitchActions)


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes f -> f bytes) imageFile glitchActions
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"
