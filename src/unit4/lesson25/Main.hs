module Unit4.Lesson25.Main where

import           Control.Monad         (foldM)
import qualified Data.ByteString.Char8 as BC
import           System.Environment    (getArgs)
import           Unit4.Lesson25.Glitch (glitchActions)


main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes f -> f bytes) imageFile glitchActions
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"
