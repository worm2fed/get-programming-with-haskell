import qualified Data.Map as Map


askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName
          >> getLine
          >>= (\name -> return $ nameStatement name)
          >>= putStrLn


helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn $ nameStatement name

echo :: IO ()
echo = do
    t <- getLine
    putStrLn t


data Grade = F | D | C | B | A
    deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD
    deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
    { candidateId :: Int
    , codeReview  :: Grade
    , cultureFit  :: Grade
    , education   :: Degree
    } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where passedCoding = codeReview candidate > B
          passedCultureFit = cultureFit candidate > C
          educationMin = education candidate >= MS
          tests = [ passedCoding
                  , passedCultureFit
                  , educationMin
                  ]

testCandidate :: Candidate
testCandidate = Candidate 1 A B PhD


readInt :: IO Int
readInt = getLine >>= return . read

readGrade :: IO Grade
readGrade = getLine >>= return . read

readDegree :: IO Degree
readDegree = do
    grade <- getLine
    return $ read grade

readCandidate :: IO Candidate
readCandidate = do
    putStrLn "enter id:"
    cId <- readInt
    putStrLn "enter code grade:"
    codeGrade <- readGrade
    putStrLn "enter culture grade:"
    cultureGrade <- readGrade
    putStrLn "enter education:"
    degree <- readDegree
    return $ Candidate cId codeGrade cultureGrade degree

assessCandidateIO :: IO String
assessCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement


candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA
                       }

candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD
                       }

candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS
                       }

candidatesDB :: Map.Map Int Candidate
candidatesDB = Map.fromList [ (1, candidate1)
                            , (2, candidate2)
                            , (3, candidate3)
                            ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
    candidate <- Map.lookup cId candidatesDB
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement

printResult :: Maybe String -> String
printResult Nothing  = "error id not found"
printResult (Just s) = s


candidates :: [Candidate]
candidates = [ candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement

assessCandidates :: [Candidate] -> [String]
assessCandidates candidates =
        map (\x -> if x then "passed" else "failed") passed
    where passed = map viable candidates

-- yes, it handles


assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed then "passed" else "failed"
    return statement


-- Q31.1 At the end of lesson 21, you saw the following
-- program used to calculate the cost of pizza:
-- main :: IO ()
-- main = do
--     putStrLn "What is the size of pizza 1"
--     size1 <- getLine
--     putStrLn "What is the cost of pizza 1"
--     cost1 <- getLine
--     putStrLn "What is the size of pizza 2"
--     size2 <-  getLine
--     putStrLn "What is the cost of pizza 2"
--     cost2 <- getLine
--     let pizza1 = (read size1, read cost1)
--     let pizza2 = (read size2, read cost2)
--     let betterPizza = comparePizzas pizza1 pizza2
--     putStrLn (describePizza betterPizza)
-- Desugar this code to use >>=, >>, return and lambda functions rather than do-notation.
comparePizzas :: (Double, Double) -> (Double, Double) -> (Double, Double)
comparePizzas = undefined
describePizza :: (Double, Double) -> String
describePizza = undefined

main :: IO ()
main = putStrLn "What is the size of pizza 1" >> getLine
    >>= (\size1 ->
            putStrLn "What is the cost of pizza 1" >> getLine
        >>= (\cost1 ->
                putStrLn "What is the size of pizza 2" >> getLine
            >>= (\size2 ->
                    putStrLn "What is the cost of pizza 2" >> getLine
                >>= (\cost2 ->
                        putStrLn . describePizza $ comparePizzas
                            (read size1, read cost1)
                            (read size2, read cost2)
                    )
                )
            )
        )


-- Q31.2 At the end of lesson 21 in unit 4, we first
-- introduced the idea that do-notation isn’t specific to
-- IO. You ended up with this function for a Maybe type:
--  maybeMain :: Maybe String
--  maybeMain = do
--     size1 <- Map.lookup 1 sizeData
--     cost1 <- Map.lookup 1 costData
--     size2 <- Map.lookup 2 sizeData
--     cost2 <- Map.lookup 2 costData
--     let pizza1 = (size1,cost1)
--     let pizza2 = (size2,cost2)
--     let betterPizza = comparePizzas pizza1 pizza2
--     return  (describePizza betterPizza)
-- Rewrite this function so it works with the List type (don’t worry if the results seem strange).
costData, sizeData :: [a]
costData = undefined
sizeData = undefined

listMain :: [String]
listMain = do
    size1 <- [20, 15]
    cost1 <- [18.0, 16.0]
    size2 <- [2, 51]
    cost2 <- [81.0, 61.0]
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)


-- Q31.3 Refactor the maybeMain function from the preceding
-- exercise so that it works with any Monad. You’ll need to
-- change the type signature as well as remove the
-- type-specific parts from the body of the function.
anyMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
anyMain s1 c1 s2 c2= do
    size1 <- s1
    cost1 <- c1
    size2 <- s2
    cost2 <- c2
    let pizza1 = (size1, cost1)
    let pizza2 = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)
