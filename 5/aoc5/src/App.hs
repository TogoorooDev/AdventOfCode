module App
    ( main,
      Crate (..),
      Stack
    ) where

import System.IO

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

cutLine' :: (Integral a) => a -> String -> [String] -> [String]
cutLine' _ [] _ = []
cutLine' n xs ys
    | length sec <= nn = [first] <> [sec] <> ys
    | otherwise = cutLine' nn sec ([first] <> ys)
    where 
        nn = fromIntegral n
        first = take nn xs
        sec = drop nn xs

cutLine :: Integer -> String -> [String]
cutLine n xs = cutLine' n xs []
    

genCrateStack :: Integer -> Integer -> [String] -> [Crate]
genCrateStack _ _ [] = []
genCrateStack n nn (x:xs) = [Crate { name = x !! 1, stack = n, position = nn }] <> genCrateStack n (nn + 1) xs    

main :: IO ()
main = withFile "../input.txt" ReadMode $ \h -> do
    hIn <- hGetContents h
    let l = lines hIn
    let v = isolateValues l
    let vv = map (cutLine 4) v
    -- print vvv
    -- let mode = l !! length v
    
    -- let iFirst = read [mode !! 1] :: Integer
    -- let iLast = read [mode !! (length mode - 2)] :: Integer


    -- let alln = [iFirst..iLast]

    let q = zip vv [1..] :: [([String], Integer)]

    let vvvv = map (\x -> genCrateStack (snd x) 1 (fst x)) q
    let vvvvv = map (filter (\x -> name x /= ' ')) vvvv
    -- let vvvv = genCrateStack 0 0 q


    
    print vvvvv
    
    
