module Algorithm where

import Board
import Tools

data GameTree = Node {turn :: Turn, brd :: Board, tab :: [GameTree]} deriving Show

isWolfBehindThreeSheep :: Board -> Bool
isWolfBehindThreeSheep (Board w s) = (countNumberOfSheepBehindWolf w s) >= 3
    where
        countNumberOfSheepBehindWolf w s = length ( filter (> yWolf) ySheep )
                                        where
                                            yWolf = snd (pfield w)
                                            ySheep = (map snd (map pfield s))

countMeanDistanceToAllSheep :: Floating a => Piece -> [Piece] -> a
countMeanDistanceToAllSheep w [] = 0
countMeanDistanceToAllSheep w (s:xs) = sqrt (fromIntegral ((x1 - x2)^2 + (y1 - y2)^2))
                                             + countMeanDistanceToAllSheep w xs
                                                    where
                                                        x1 = fst (pfield w)
                                                        x2 = fst (pfield s)
                                                        y1 = snd (pfield w)
                                                        y2 = snd (pfield s)

permutationsOfAllSheep :: [Piece] -> Board -> [[(Int, Int)]]
permutationsOfAllSheep [] _ = []
permutationsOfAllSheep (s:xs) b = (bigPermutationForOneSheep (possibleMoves s b) s b) ++ permutationsOfAllSheep xs b
                    where
                    bigPermutationForOneSheep [] _ _ = []
                    bigPermutationForOneSheep (newPosition:xp) s b = [(smallPermutationForOneSheep s newPosition b)]
                        ++ (bigPermutationForOneSheep xp s b)
                            where
                                smallPermutationForOneSheep x newSheepPos b = filter (/= (pfield x)) (newSheepPos : (map pfield (sheep b)))

permutationsOfWolf :: Piece -> Board -> [(Int, Int)]
permutationsOfWolf w b = possibleMoves w b

mapSheepToBoard :: Piece -> [[(Int, Int)]] -> [Board]
mapSheepToBoard w s = map (\z -> Board w z) (map (\x ->  map (\y -> Sheep y) x) s)

mapWolfToBoard :: [(Int, Int)] -> [Piece] -> [Board]
mapWolfToBoard w s = map (\z -> Board z s) (map (\x -> Wolf x) w)

oppositeTurn WolfTurn = SheepTurn
oppositeTurn SheepTurn = WolfTurn

generateBoard :: Board -> Turn -> [Board]
generateBoard b t | t == SheepTurn = mapSheepToBoard (wolf b) (permutationsOfAllSheep (sheep b) b)
                  | t == WolfTurn = mapWolfToBoard (permutationsOfWolf (wolf b) b) (sheep b)

generateNode :: Board -> Turn -> Int -> [Board] -> GameTree
generateNode b t _ [] = Node t b []
generateNode b t n listaNextow | n < 5 && (result b) == Unconcluded = Node t b (map (\x->(generateNode x (oppositeTurn t) (n+1) (generateBoard x (oppositeTurn t)))) listaNextow)
                               | otherwise = Node t b []

rateNode :: GameTree -> Int
rateNode (Node _ b _) = beingOnLastRow + beingBehindThreeSheep + beingFarFromEdges + beingFarFromSheep + goingForward + isWolfWithNoMoves
            where
                  x = fst (pfield (wolf b))
                  y = snd (pfield (wolf b))
                  beingOnLastRow | y == 0 = 10000 *
                                 | otherwise  = 0
                  beingBehindThreeSheep | isWolfBehindThreeSheep b = 5000
                                        | otherwise  = 0
                  beingFarFromEdges     | x > 0 && x < 7 && y > 0 && y < 7 = 1000
                                        | otherwise = 0
                  beingFarFromSheep = round (countMeanDistanceToAllSheep (wolf b) (sheep b))
                  goingForward = (7 - y) * 100
                  isWolfWithNoMoves | possibleMoves (wolf b) b == [] = -100000
                                    | otherwise = 0


rateTree :: GameTree -> Int
rateTree (Node t b []) = -1
rateTree (Node t b treeList) = snd (maxim (map (\y -> rateTreeSub y) treeList))

rateTreeSub (Node t b treeList)  | length treeList /= 0 && t == WolfTurn = maximum tab
                                 | length treeList /= 0 && t == SheepTurn = minimum tab
                                 | otherwise = rateNode (Node t b treeList)
                                 where
                                 tab = (map (\y -> rateTreeSub y) treeList)

getOptimalMove :: Board -> Board
getOptimalMove b =
    let x = generateNode b WolfTurn 0 (generateBoard b WolfTurn)
    in
        if (rateTree x) /= (-1)
        then
            brd((tab x) !! (rateTree x))
        else
            b

-------

test (Node t b treeList) = map (\y -> rateTreeSub y) treeList

b = Board (Wolf(4,1)) [Sheep(1,2), Sheep(3,4), Sheep(4,3), Sheep(6,7)]
t = generateNode b WolfTurn 0 (generateBoard b WolfTurn)
o = getOptimalMove b



-- b = Board (Wolf(6,0)) [Sheep(2,7), Sheep(3,6), Sheep(5,6), Sheep(7,6)]



