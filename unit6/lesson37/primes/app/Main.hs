module Main where

import           Data.Maybe (isNothing)
import           Primes


checkAction :: IO ()
checkAction = do
  putStrLn "Enter a number to check if it's prime:"
  n <- getLine
  let result = isPrime (read n)
  case result of
    Nothing -> putStrLn "Sorry, this number is not a valid candidate for primality testinge"
    Just x -> if x
              then putStrLn "It's prime"
              else putStrLn "It's not prime"

factorAction :: IO ()
factorAction = do
  putStrLn "Enter a number to Factor:"
  n <- getLine
  let result = primeFactors (read n)
  if isNothing result
  then putStrLn "Sorry, this number is not a valid candidate for primality testinge"
  else print result

main :: IO ()
main = do
  checkAction
  factorAction
