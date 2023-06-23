module App
    ( main,
      Crate (..),
      Stack
    ) where

import System.IO
import Helper

data Crate = Crate 
    {
    name :: Char, 
    stack :: Integer, 
    position :: Integer
    } 
    deriving (Show, Eq)

type Stack = [Crate]
    
isolateValues' :: [String] -> [String] -> [String]
isolateValues' [] _ = []
isolateValues' (x:xs) ys
    | '[' `notElem` x = ys
    | otherwise = isolateValues' xs (ys <> [x <> " "])

isolateValues :: [String] -> [String]
isolateValues x = isolateValues' x []

genCrateStack :: Integer -> Integer -> [String] -> [Crate]
genCrateStack _ _ [] = []
genCrateStack n nn (x:xs) = [Crate { name = x !! 1, stack = n, position = nn }] <> genCrateStack n (nn + 1) xs    

main :: IO ()
main = withFile "../input.txt" ReadMode $ \h -> do
    hIn <- hGetContents h
    let l = lines hIn
    let v = isolateValues l
    let vv = map (cutLine 4) v

    let vvvv = map (\x -> genCrateStack (snd x) 1 (fst x)) q
    let vvvvv = map (filter (\x -> name x /= ' ')) vvvv
    -- let vvvv = genCrateStack 0 0 q


    print vvvv
    
    
