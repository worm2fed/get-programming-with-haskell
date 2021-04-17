module Main where

import           Control.Monad              (forM_)
import           Data.Aeson
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text                  as T
import           GHC.Generics


data NOAAResult = NOAAResult
  { uid          :: T.Text
  , mindate      :: T.Text
  , maxdate      :: T.Text
  , name         :: T.Text
  , datacoverage :: Double
  , resultId     :: T.Text
  } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"

instance ToJSON NOAAResult where
  toJSON noaaResult =
    object [ "uid"          .= uid noaaResult
           , "mindate"      .= mindate noaaResult
           , "maxdate"      .= maxdate noaaResult
           , "name"         .= name noaaResult
           , "datacoverage" .= datacoverage noaaResult
           , "id"           .= resultId noaaResult
           ]

data Resultset = Resultset
  { offset :: Int
  , count  :: Int
  , limit  :: Int
  } deriving (Show, Generic)

instance FromJSON Resultset
instance ToJSON Resultset

newtype Metadata = Metadata
  { resultset :: Resultset
  } deriving (Show, Generic)

instance FromJSON Metadata
instance ToJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results  :: [NOAAResult]
  } deriving (Show, Generic)

instance FromJSON NOAAResponse
instance ToJSON NOAAResponse


printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
  forM_ results (print . name)


main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults
