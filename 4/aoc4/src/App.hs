module App
    ( main
    ) where

import System.IO
import Data.List.Split (splitOn)

genSet :: String -> (String, String)
genSet xs = do
    let l = splitOn "," xs
    (l !! 0, l !! 1)

_getFullValues :: String -> [Integer]
_getFullValues xs = do
    let pair = splitOn "-" xs
    [(read (pair !! 0))..(read (pair !! 1))]

getFullValues :: (String, String) -> ([Integer], [Integer])
getFullValues x = (_getFullValues (fst x), _getFullValues (snd x))

isOverlapping :: ([Integer], [Integer]) -> Bool
isOverlapping x
    | (head (fst x) <= head (snd x)) && (last (fst x ) >= last (snd x)) = True
    | (head (snd x) <= head (fst x)) && (last (snd x ) >= last (fst x)) = True
    | otherwise = False

_isPartiallyOverlapping :: [Integer] -> [Integer] -> Bool
_isPartiallyOverlapping [] _ = False
_isPartiallyOverlapping (x:xs) ys 
                            | filter (x ==) ys /= [] = True
                            | otherwise = _isPartiallyOverlapping xs ys

isPartiallyOverlapping :: ([Integer], [Integer]) -> Bool
isPartiallyOverlapping xx = _isPartiallyOverlapping (fst xx) (snd xx)


-- isPartiallyOverlapping :: ([Integer], [Integer]) -> Bool
-- isPartiallyOverlapping x
--     | (head (fst x) <= head (snd x)) && (last (fst x ) >= last (snd x)) = True
--     | (head (snd x) <= head (fst x)) && (last (snd x ) >= last (fst x)) = True
--     | otherwise = False

main :: IO ()
main = withFile "../input.txt" ReadMode $ \h -> do
    hIn <- hGetContents h
    let pairs = lines hIn
    let set = map (genSet) pairs
    let l = map (getFullValues) set
    let overlapping = filter (isPartiallyOverlapping) l

    -- map (\x -> print '\n' ++ x) overlapping

    print $ length overlapping
    
