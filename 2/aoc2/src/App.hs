module App
    ( main
    ) where

-- import Split
import System.IO

{-
   Format: 
   
  'Q R'
   Q -> A B C
   R -> X Y Z

   Meanings:
   A, X = Rock
   B, Y = Paper
   C, Z = Scissors 

   Scoring:
      A ~ X
      B ~ Y
      C ~ Z
         += 3

      A ~ Y
      B ~ Z
      C ~ X
         += 0

      A ~ Z
      B ~ X
      C ~ Y
         += 6

   Bonus:
   A += 1
   B += 2
   C += 3  
-}

data Hand = Rock | Paper | Scissors
   deriving (Show, Eq, Enum)

data State = Lose | Tie | Win
   deriving (Show, Eq, Enum)

data Round = Round { them :: Hand, you :: Hand} 
   deriving (Show, Eq)

instance Ord Hand where 
   (<=) x y
      | x == Scissors && y == Paper = False
      | x == Rock && y == Scissors = False
      | x == Paper && y == Rock = False
      | otherwise = True


roundLost :: Round -> Bool
roundLost x = (you x) <= (them x)

roundTie :: Round -> Bool
roundTie x = (you x) == (them x)

roundCalc :: Round -> Integer
roundCalc x 
   | roundTie x = 3
   | roundLost x = 0
   | otherwise = 6

bonusCalc :: Round -> Integer
bonusCalc x = bonusCalcH (you x)

bonusCalcH :: Hand -> Integer
bonusCalcH x 
   | x == Rock = 1
   | x == Paper = 2
   | x == Scissors = 3
   | otherwise = 0

roundScore :: Round -> Integer
roundScore x = (roundCalc x) + (bonusCalc x)

getSides :: String -> (Char, Char)
getSides x = (x !! 0, x !! 2)

charToHandConv :: Char -> Hand
charToHandConv x
   | x == 'A' || x == 'X' = Rock
   | x == 'B' || x == 'Y' = Paper
   | x == 'C' || x == 'Z' = Scissors
   | otherwise = Rock

charToPosState :: Char -> State
charToPosState x
   | x == 'X' = Lose
   | x == 'Y' = Tie
   | x == 'Z' = Win
   | otherwise = Lose

fillPos :: Hand -> State -> Hand
fillPos x y = case x of 
      Rock -> case y of
         Lose -> Scissors
         Tie -> Rock
         Win -> Paper
      Paper -> case y of
         Lose -> Rock
         Tie -> Paper
         Win -> Scissors
      Scissors -> case y of
         Lose -> Paper
         Tie -> Scissors
         Win -> Rock   
         
mkRoundVar :: [(Char, Char)] -> [Round]
mkRoundVar [] = []
mkRoundVar (x:xs) = do
   let t = charToHandConv (fst x)
   let y = fillPos t (charToPosState (snd x))

   [Round {them = t, you = y}] ++ mkRoundVar xs

   -- [Round {them = (charToHandConv $ fst x), you = ( fillPos (charToHandConv $ fst x) $ charToPosState $ snd x)}] ++ (mkRoundVar xs) 

main :: IO ()
main = withFile "../input.txt" ReadMode $ \h -> do
   hIn <- hGetContents h
   let sides = lines hIn
   let ssides = map (getSides) sides
   let rounds = mkRoundVar ssides
   print rounds
   
   let scores = map (roundScore) rounds

   print (sum scores)