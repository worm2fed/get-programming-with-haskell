import           Data.Char                 (isPunctuation, isSpace, toLower)
import qualified Data.Text                 as T
import           Lib
import           Test.QuickCheck
import           Test.QuickCheck.Instances


prop_punctuationInvariant text = preProcess text == allSmall
    where noPunctuationText = T.filter (not . isPunctuation) text
          noWhiteSpacesText = T.filter (not . isSpace) noPunctuationText
          allSmall = T.map toLower noWhiteSpacesText

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)


main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
    quickCheck prop_reverseInvariant
    putStrLn "done!"

