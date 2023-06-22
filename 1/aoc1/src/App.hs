module App
   ( main
   ) where

import System.IO
import Control.Monad.State
import Data.Foldable
import Data.List (sort)

startState :: ([a] , [[a]])
startState = ([], [[]])
 
splitState :: Eq a => a -> [a] -> State ([a], [[a]]) [[a]]
splitState p xs = do
   for_ xs $ \x -> do 
      (partial, arr) <- get
      if x == p then do
         put ([], partial:arr)
      else do
         put (partial ++ [x], arr)

   (_, rarr) <- get
   return rarr

split :: Eq a => a -> [a] -> [[a]]
split p xs = evalState (splitState p xs) startState

splitByEmptyElement :: [String] -> [[String]]
splitByEmptyElement x = split "" x

intStrings :: [String] -> [Integer]
intStrings l = map read l

-- csumInts :: [Integer] -> Integer


main :: IO ()
main = withFile "../input" ReadMode $ \h -> do
   hIn <- hGetContents h
   let stringsList = filter (/= []) $ splitByEmptyElement $ split '\n' hIn
   let intList = map (intStrings) stringsList 
   let sumList = map (sum) intList
   let sorted = reverse (sort sumList)
   let top3 = take 3 sorted
   print ( sum top3 )
