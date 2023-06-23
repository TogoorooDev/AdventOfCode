module Parse 
   ( parseTxt
   ) where

import Helper

-- move X from Y to Z
data Action = Action 
   {
   amount :: Integer,
   start :: Integer,
   end :: Integer
   }
   deriving (Show, Eq, Ord)

parseTxt :: String -> Action
parseTxt x = Action {
      amount = read (w !! 1),
      start = read (w !! 3),
      end = read (last w)
   } where w = words x


-- replaceElem xs x n = s ++ [x] ++ e
--    where
--       nn = fromIntegral n
--       nnn = castCCharToChare nn of 
--          0 -> 0
--          _ -> nn
--       s = take (nnn-1) xs
--       e = drop (nnn) xs
      

-- modifyCrate :: [Stack] -> Action -> [Stack]
-- modifyCrate xs a = do
--    let sstart = xs !! start a
--    let send = xs !! end a

--    let newstacks = modifyCrate' xs sstart send a (amount a)

--    map (replaceElem xs x) newstacks
   

-- modifyCrate' :: Stack -> Stack -> Action -> Integer -> (Stack, Integer)
-- modifyCrate' start end a n = do

   