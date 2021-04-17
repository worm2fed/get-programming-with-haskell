{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text                  as T
import           GHC.Generics


-- because there can not be an error

data Book = Book
  { title  :: T.Text
  , author :: T.Text
  , year   :: Int
  } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book
  { author = "Will Kurt"
  , title = "Learn Haskell"
  , year = 2017
  }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

data Name = Name
  { firstName :: T.Text
  , lastName  :: T.Text
  } deriving (Show, Generic)

instance FromJSON Name where
  parseJSON (Object v) =
    Name <$> v .: "firstName"
         <*> v .: "lastName"

instance ToJSON Name where
  toJSON (Name firstName lastName) =
    object [ "firstName" .= firstName
           , "lastName"  .= lastName
           ]


sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage
  { message   :: T.Text
  , errorCode :: Int
  } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage <$> v .: "message"
                 <*> v .: "error"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object [ "message" .= message
           , "error"   .= errorCode
           ]

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0


-- Q40.1 Make your NOAAResponse type an instance of
-- ToJSON. This requires making all the types used
-- by this type instances of ToJSON as well.


-- Q40.2 Make a Sum type called IntList and use
-- DerivingGeneric to make it an instance of ToJSON.
-- Don’t use the existing List type, but rather write
-- it from scratch. Here’s an exam- ple of an IntList:
intListExample :: IntList
intListExample = Cons 1 $
                 Cons 2 EmptyList

data IntList = EmptyList | Cons Int IntList
  deriving (Generic)
instance ToJSON IntList
