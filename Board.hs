module Board where

import Data.List
import Data.Maybe
import Data.Char


data Piece = Wolf { pfield :: (Int, Int) } | Sheep { pfield :: (Int, Int) } deriving (Eq, Show)
data Board = Board { wolf :: Piece,  sheep :: [Piece] } deriving Eq

data Result = Unconcluded | SheepWon | WolfWon deriving Show

data Direction = L | R deriving (Eq, Show)

dir :: [Char] -> Direction
dir "l" = L
dir "r" = R

pf :: Board -> (Int, Int) -> Char
pf board (x,y)
  | pfield (wolf board) == (x,y) = 'W'
  | not (isNothing i) = show (fromJust i + 1) !! 0
  | otherwise = '.' where
      i = elemIndex (x,y) [pfield s | s <- sheep board]

printBoard :: Board -> String
printBoard board =
  unlines ( header : [row board j | j <- [0..7]] ) where 
  row b r = intercalate " " (show r : [ [pf board (x, r)] | x <- [0..7]])
  header = "  0 1 2 3 4 5 6 7"

instance Show Board where
  show (Board w s) = printBoard(Board w s)

pieces :: Board -> [(Int,Int)]
pieces b =
  pfield (wolf b) : [pfield s | s <- sheep b]

moves :: Piece -> [(Int, Int)]
moves (Wolf (x1,y1)) =
  [  (x2,y2)
  | x2 <- [ x1 + 1, x1 - 1 ],
    y2 <- [ y1 -1, y1 + 1 ],
   x2 <= 7,
   x2 >= 0,
   y2 <= 7,
   y2 >= 0
  ]

moves (Sheep (x1,y1)) =
  [  (x2,y2)
  | x2 <- [ x1 + 1, x1 - 1 ],
    y2 <- [ y1 + 1 ],
   x2 <= 7,
   x2 >= 0,
   y2 <= 7,
   y2 >= 0
  ]

possibleMoves :: Piece -> Board -> [(Int, Int)]
possibleMoves p b =
  [ m |
    m <- moves p,
    not (elem m (pieces b)) ]


result :: Board -> Result
result b
  | snd (pfield (wolf b)) == 0 = WolfWon
  | possibleMoves (wolf b) b == [] = SheepWon
  | otherwise = Unconcluded


moveWolf :: (Int, Int) -> Board -> Board

moveWolf (x,y) b 
  | (x,y) `elem` possibleMoves (wolf b) b = Board (Wolf (x,y)) (sheep b)
  | otherwise = b

moveSheep :: Direction ->  Int -> Board -> Board
moveSheep d i b
  | (x,y) `elem` possibleMoves (sheep b !! (i - 1)) b =
    Board (wolf b) [ if i - 1 == j then (Sheep (x,y)) else sheep b !! j | j <-
                       [0..(length (sheep b) - 1)]]
  | otherwise = b
  where 
    (x,y) = if d == L
            then (x'-1,y'+1)
            else (x'+1,y'+1)
      where
        (x',y') = pfield $ sheep b !! (i - 1)

