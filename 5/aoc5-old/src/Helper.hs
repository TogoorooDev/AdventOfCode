module Helper 
   ( cutLine,
     deepIndex,
     chopLists,
     replaceElem
   ) where

replaceElem :: [a] -> a -> Integer -> [a]
replaceElem xs x n = do
   let nn = fromIntegral n
   let (y,z) = splitAt (nn + 1) xs
   let q = init y ++ [x]
   q ++ z

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

-- chopLists' :: Ord a => [a] -> [a] -> [a]
-- chopLists' 

deepIndex :: (Ord a, Integral b) => [[a]] -> b -> [a]
-- deepIndex [[]] _ = [[]]
deepIndex [] _ = []
deepIndex (x:xs) n = x !! nn : deepIndex xs n
    where 
        nn = fromIntegral n

getMinimumListLength' :: (Ord a, Integral b) => [[a]] -> b -> b
getMinimumListLength' [] lowest = lowest
getMinimumListLength' (x:xs) lowest
    | curLen < lowest = getMinimumListLength' xs curLen
    | otherwise = getMinimumListLength' xs lowest
    where 
        curLen = fromIntegral $ length x

getMinimumListLength :: (Ord a, Integral b) => [[a]] -> b
getMinimumListLength xs = getMinimumListLength' xs n
    where 
        n = fromIntegral $ length $ head xs
        
chopLists' :: (Ord a, Integral b) => [[a]] -> b -> [[a]]
chopLists' [[]] _ = [[]]
chopLists' [] _ = []
chopLists' xs n
    | n > 0 = chopLists' xs nn : deepIndex xs nn
    | otherwise = xs
    where 
        nn = n - 1

chopLists :: (Ord a) => [[a]] -> [[a]]
chopLists xs = chopLists' xs $ getMinimumListLength xs