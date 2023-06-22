module Helper 
   ( cutLine,
     deepIndex,
     chopLists
   ) where


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

deepIndex :: (Ord a, Integral b) => [[a]] -> b -> [[a]]
-- deepIndex [[]] _ = [[]]
deepIndex [] _ = []
deepIndex (x:xs) n = [x !! nn] : deepIndex xs nn - 1
    where 
        nn = fromIntegral n

chopLists :: (Ord a, Integral b) => [[a]] -> b -> [[a]]
chopLists [[]] _ = [[]]
chopLists [] _ = []
chopLists _ 0 = []
chopLists xs n = [deepIndex xs n] : chopLists xs n - 1
