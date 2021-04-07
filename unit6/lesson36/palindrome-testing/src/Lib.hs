module Lib
    ( isPalindrome
    , preProcess
    ) where

import           Data.Char (isPunctuation, isSpace, toLower)
import qualified Data.Text as T


stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

toLowerCase :: T.Text -> T.Text
toLowerCase = T.map toLower

preProcess :: T.Text -> T.Text
preProcess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preProcess text

