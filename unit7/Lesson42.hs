import           Control.Monad      (forM_, when)
import           Control.Monad.ST   (ST)
import           Data.Array.ST      (MArray (newArray), STUArray, readArray,
                                     runSTUArray, thaw, writeArray)
import           Data.Array.Unboxed (UArray, accum, array, bounds, listArray,
                                     (!), (//))


aLargeList :: [Int]
aLargeList = [1 .. 10000000]

aLargeArray :: UArray Int Int
aLargeArray = array (0, 9999999) []

aLargeListDoubled :: [Int]
aLargeListDoubled = map (* 2) aLargeList

zeroIndexArray :: UArray Integer Bool
zeroIndexArray = array (0, 9) [(3, True)]

oneIndexArray :: UArray Integer Bool
oneIndexArray = array (1, 10) $ zip [1 .. 10] $ repeat True

qcArray :: UArray Int Bool
qcArray = array (0, 4) [(0, True), (1, True), (2, True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) $ zip [0 .. 3] $ repeat 0

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1, 5), (3, 6)]

doubleBib :: UArray Int Int
doubleBib = accum (*) updatedBiB $ zip [0 .. 3] $ repeat 2


listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray values = do
  let end = length values - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = values !! i
    writeArray myArray i val
  return myArray


listToUArray :: [Int] -> UArray Int Int
listToUArray values = runSTUArray $ listToSTUArray values

listToUArrayRefactored :: [Int] -> UArray Int Int
listToUArrayRefactored values = runSTUArray $ do
  let end = length values - 1
  myArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = values !! i
    writeArray myArray i val
  return myArray


myData :: UArray Int Int
myData = listArray (0, 5) [7,6,4,8,10,2]

myData' :: UArray Int Int
myData' = listToUArrayRefactored [7,6,4,8,10,2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. end - i] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray

-- Q42.1 One of the most important operations in the
-- implementation of a genetic algo- rithm is combining
-- two arrays of Booleans through an operation called
-- crossover. Cross- over takes as input a pair of
-- equal-sized arrays. Then a cutoff point is chosen, and
-- the top and bottom are swapped. The final value is this
-- new pair of arrays. Here’s an illus- tration using lists
-- and an example (using 1 for True and 0 for False):
--  ([1,1,1,1,1],[0,0,0,0,0])
-- If you perform crossover at index 3, your result should be
-- [1,1,1,0,0]
-- Implement crossover where the result is a UArray but the
-- crossover itself is performed using STUArrays
crossover :: (UArray Int Int, UArray Int Int) -> Int -> UArray Int Int
crossover (a, b) point = runSTUArray $ do
  stA <- thaw a
  let end = (snd . bounds) a
  forM_ [point .. end] $ \i -> do
    writeArray stA i $ b ! i
  return stA


-- Q42.2 Write a function that takes a UArray Int Int
-- as an input. The input will have a mixture of zeros
-- and other values. The function, replaceZeros, should
-- return the array with all of the zeros replaced with
-- the value –1.
cutZeros :: UArray Int Int -> UArray Int Int
cutZeros a = runSTUArray $ do
  stArray <- thaw a
  let end = (snd . bounds) a
  forM_ [1 .. end] $ \i -> do
    val <- readArray stArray i
    when (val == 0) $ do
      writeArray stArray i (-1)
  return stArray
