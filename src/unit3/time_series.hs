import           Data.List
import qualified Data.Map       as Map
import           Data.Maybe
import           Data.Semigroup


file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2, 199.5), (3, 199.4)
        , (4, 198.9), (5, 199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)
        ]

file2 :: [(Int,Double)]
file2 = [ (11, 201.6), (12, 201.5), (13, 201.5)
        , (14, 203.5), (15, 204.9), (16, 207.1)
        , (18, 210.5), (20, 208.8)
        ]

file3 :: [(Int,Double)]
file3 = [ (10, 201.2), (11, 201.6), (12, 201.5)
        , (13, 201.5), (14, 203.5), (17, 210.5)
        , (24, 215.1), (25, 218.7)
         ]

file4 :: [(Int,Double)]
file4 = [ (26, 219.8), (27, 220.5), (28, 223.8)
        , (29, 222.8), (30, 223.8), (31, 221.7)
        , (32, 222.3), (33, 220.8), (34, 219.4)
        , (35, 220.1), (36, 220.6)
        ]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
    where completeTimes = [minimum times .. maximum times]
          timeValueMap = Map.fromList (zip times values)
          extendedValues = map (`Map.lookup` timeValueMap) completeTimes

fileToTs :: [(Int, a)] -> TS a
fileToTs tvPairs = createTS times values
    where (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing      = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
        where rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTs file1

ts2 :: TS Double
ts2 = fileToTs file2

ts3 :: TS Double
ts3 = fileToTs file3

ts4 :: TS Double
ts4 = fileToTs file4


insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing)      = myMap
insertMaybePair myMap (key, Just value) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where bothTimes = mconcat [t1, t2]
          completeTimes = [minimum bothTimes .. maximum bothTimes]
          tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
          updateMap = foldl insertMaybePair tvMap (zip t2 v2)
          combinedValues = map (`Map.lookup` updateMap) completeTimes

instance Semigroup (TS a) where
    (<>) = combineTS

instance Monoid (TS a) where
    mempty = TS [] []

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]


avgTS :: Real a => ([a] -> Double) -> TS a -> Maybe Double
avgTS _ (TS _ []) = Nothing
avgTS f (TS times values) = if all (== Nothing) values
                           then Nothing
                           else Just avg
    where justValues = filter isJust values
          cleanValues = map fromJust justValues
          avg = f cleanValues

mean :: Real a => [a] -> Double
mean xs = total / count
    where total = (realToFrac . sum) xs
          count = (realToFrac . length) xs

meanTS :: Real a => TS a -> Maybe Double
meanTS = avgTS mean


median :: Real a => [a] -> Double
median xs = if even listLength
            then evenElement / 2
            else oddElement
    where listLength = length xs
          getElement i = realToFrac (sort xs !! i)
          oddElement = getElement (listLength `div` 2)
          evenElement = oddElement +
            getElement ((listLength `div` 2) - 1)

medianTS :: Real a => TS a -> Maybe Double
medianTS = avgTS median


type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = newF
    where newF (i1, Nothing) (i2, Nothing)     = (i1, Nothing)
          newF (_, Nothing) (i, val)           = (i, val)
          newF (i, val) (_, Nothing)           = (i, val)
          newF (i1, Just val1) (i2, Just val2) =
              if f val1 val2 == val1
              then (i1, Just val1)
              else (i2, Just val2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS f (TS times values) = if all (== Nothing) values
                                then Nothing
                                else Just best
    where pairs = zip times values
          best = foldl (makeTSCompare f) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _         = Nothing
diffPair _ Nothing         = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
    where shiftValues = tail values
          diffValues = zipWith diffPair shiftValues values


avgMaybe :: Real a => ([a] -> Double) -> [Maybe a] -> Maybe Double
avgMaybe f values = if any (== Nothing) values
                    then Nothing
                    else Just avg
    where avg = f (map fromJust values)

medianMaybe :: Real a => [Maybe a] -> Maybe Double
medianMaybe = avgMaybe median

meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe = avgMaybe mean


movingAvg :: Real a => ([Maybe a] -> Maybe Double) -> [Maybe a] -> Int -> [Maybe Double]
movingAvg _ [] _ = []
movingAvg f values n = if length nextValues == n
                       then f nextValues : movingAvg f restValues n
                       else []
    where nextValues = take n values
          restValues = tail values

movingMeanAvg :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingMeanAvg = movingAvg meanMaybe

movingMedianAvg :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingMedianAvg = movingAvg medianMaybe


movingAverageTS :: Real a => ([Maybe a] -> Int -> [Maybe Double]) -> TS a -> Int -> TS Double
movingAverageTS _ (TS [] []) _ = TS [] []
movingAverageTS f (TS times values) n = TS times smoothedValues
    where ma = f values n
          nothings = replicate (n `div` 2) Nothing
          smoothedValues = mconcat [nothings, ma, nothings]

movingMeanAverageTS :: Real a => TS a -> Int -> TS Double
movingMeanAverageTS  = movingAverageTS movingMeanAvg

movingMedianAverageTS :: Real a => TS a -> Int -> TS Double
movingMedianAverageTS  = movingAverageTS movingMedianAvg
