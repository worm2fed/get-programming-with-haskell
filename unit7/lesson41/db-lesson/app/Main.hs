module Main where

import           Control.Applicative
import           Data.Time
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


data Tool = Tool
  { toolId        :: Int
  , name          :: String
  , description   :: String
  , lastReturned  :: Day
  , timesBorrowed :: Int
  }

instance Show Tool where
  show tool = mconcat [ show $ toolId tool
                      , ".) "
                      , name tool
                      , "\n description: "
                      , description tool
                      , "\n last returned: "
                      , show $ lastReturned tool
                      , "\n times borrowed: "
                      , show $ timesBorrowed tool
                      , "\n"
                      ]

data User = User
  { userId   :: Int
  , userName :: String
  }

instance Show User where
  show user = mconcat [ show $ userId user
                      , ".) "
                      , userName user
                      ]


withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open "tools.db"
  action conn
  close conn

addUser :: String -> IO ()
addUser userName = withConn "tools.db" $
  \conn -> do
    execute conn
      "INSERT INTO users (username) VALUES (?)"
      (Only userName)
    print "user added"

addTool :: String -> String -> IO ()
addTool name description = withConn "tools.db" $
  \conn -> do
    currentDay <- utctDay <$> getCurrentTime
    execute conn
      "INSERT INTO tool (name, description, lastReturned, timesBorrowed) VALUES (?, ?, ?, 0)"
      (name, description, currentDay)
    print "tool added"


checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
  \conn -> do
    execute conn
      "INSERT INTO checkedout (user_id, tool_id) VALUES (?, ?)"
      (userId, toolId)
    print "user added"


instance FromRow User where
  fromRow = User <$> field
                 <*> field

instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

printUsers :: IO ()
printUsers = withConn "tools.db" $
  \conn -> do
    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
  \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $
  mconcat [ "SELECT * FROM tools "
          , "WHERE id NOT IN "
          , "(SELECT tool_id FROM checkedout);"
          ]

printCheckedout :: IO ()
printCheckedout = printToolQuery $
  mconcat [ "SELECT * FROM tools "
          , "WHERE id IN "
          , "(SELECT tool_id FROM checkedout);"
          ]


selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn
    "SELECT * FROM tools WHERE id = (?)"
    (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing []    = Nothing
firstOrNothing (x:_) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
  { lastReturned = date
  , timesBorrowed = 1 + timesBorrowed tool
  }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
  \conn -> do
    let q = mconcat [ "UPDATE tools SET "
                    , "lastReturned = ?, "
                    , "timesBorrowed = ? "
                    , "WHERE id = ?;"
                    ]
    execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
    print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
  \conn -> do
    tool <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool
                                 <*> pure currentDay
    updateOrWarn updatedTool


checkin :: Int -> IO ()
checkin toolId = withConn "tool.db" $
  \conn -> do
    execute conn
      "DELETE FROM checkedout WHERE tool_id = (?);"
      (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId


promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new user name"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user"
  userId <- read <$> getLine
  print "Enter the id of the tool"
  toolId <- read <$> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  print "Enter the id of tool"
  toolId <- read <$> getLine
  checkinAndUpdate toolId

promptAndAddTool :: IO ()
promptAndAddTool = do
  print "Enter tool name"
  name <- getLine
  print "Enter tool description"
  description <- getLine
  addTool name description

performCommand :: String -> IO ()
performCommand command
  | command == "users" = printUsers >> main
  | command == "tools" = printTools >> main
  | command == "adduser" = promptAndAddUser >> main
  | command == "addtool" = promptAndAddTool >> main
  | command == "checkout" = promptAndCheckout >> main
  | command == "checkin" = promptAndCheckin >> main
  | command == "in" = printAvailable >> main
  | command == "out" = printCheckedout >> main
  | command == "quit" = print "bye!"
  | otherwise  = print "Sorry, command not found" >> main

main :: IO ()
main = do
  print "Enter a command"
  command <- getLine
  performCommand command
