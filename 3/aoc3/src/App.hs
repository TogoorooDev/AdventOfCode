module App
    ( main
    ) where

import System.IO
import Data.List (sortOn)
import Data.Char (toUpper, toLower)
-- import Data.Function (on)

-- getHalf :: Integer -> Integer
-- getHalf n = do
--     let frac = n / 2
--     round frac

data Ruckstack = Ruckstack { items :: String, key :: Char, priority :: Integer }
    deriving (Show)
    
parseStack :: String -> (String, String)
parseStack xs = splitAt ((length xs) `div` 2) xs

findSame' :: String -> String -> Char
findSame' [] _ = '!'
findSame' (x:xs) ys = if filter(x == ) ys /= "" then x
                     else (findSame' xs ys)


findAllSame :: String -> String -> [Char]
findAllSame [] _ = []
findAllSame (x:xs) ys = filter(x == ) ys ++ findAllSame xs ys
        
findSame3 :: [String] -> Char
findSame3 [] = '!'
findSame3 xs = (findAllSame (findAllSame (xs !! 0) (xs !! 1)) (xs !! 2) ) !! 0 

isUpper :: Char -> Bool
isUpper c = (toUpper c) == c

genRStack :: (Char, String) -> Ruckstack
genRStack x = Ruckstack { items = (snd x), key = (fst x), priority = getKeyValue (fst x) }

getKeyValue :: Char -> Integer
getKeyValue x
    | isUpper x = (toInteger $ fromEnum x) - 38
    | otherwise = (toInteger $ fromEnum x) - 96


-- toRuckstack :: (String, Char, Int) -> Ruckstack
-- toRuckstack x = 

getGroups :: Int -> [a] -> [[a]]
getGroups _ [] = []
getGroups n xs = [take n xs] ++ getGroups n (drop n xs)

main :: IO ()
main = withFile "../input.txt" ReadMode $ \h -> do
    hIn <- hGetContents h
    let stacks = lines hIn
    let setStack = getGroups 3 stacks
    let eqss = map (findSame3) setStack
    let vallist = map (getKeyValue) eqss
    print $ sum vallist

    -- let splitStacks = map (parseStack) stacks
    -- let keys = map (tFindSame) splitStacks
    -- let kstacks = zip keys stacks
    -- let rstacks = map (genRStack) kstacks
    -- let pris = map (priority) rstacks

     