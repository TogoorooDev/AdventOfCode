module Split 
   ( split
   ) where 

import Control.Monad.State
import Data.Foldable

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

