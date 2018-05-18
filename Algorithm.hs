module Algorithm where

import Board

data GameTree = Node {turn :: Turn, brd :: Board, tab :: [GameTree]} deriving Show

isWolfBehindThreeSheep :: Board -> Bool
isWolfBehindThreeSheep (Board w s) = (countNumberOfSheepBehindWolf w s) >= 3
    where
        countNumberOfSheepBehindWolf w s = length ( filter (>= yWolf) ySheep )
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
generateNode b t n listaNextow | n < 7 && (result b) == Unconcluded = Node t b (map (\x->(generateNode x (oppositeTurn t) (n+1) (generateBoard x (oppositeTurn t)))) listaNextow)
                               | otherwise = Node t b []

rateNode :: GameTree -> Int -> Int
rateNode (Node _ b _) n = beingOnLastRow + beingBehindThreeSheep + beingFarFromEdges + beingFarFromSheep + goingForward + isWolfWithNoMoves
            where
                  x = fst (pfield (wolf b))
                  y = snd (pfield (wolf b))
                  beingOnLastRow | y == 0 = (10-n) * 10000
                                 | otherwise  = 0
                  beingBehindThreeSheep | isWolfBehindThreeSheep b = 5000
                                        | otherwise  = 0
                  beingFarFromEdges     | x > 0 && x < 7 && y > 0 && y < 7 = 1000
                                        | otherwise = 0
                  beingFarFromSheep = round (countMeanDistanceToAllSheep (wolf b) (sheep b))
                  goingForward = (7-y) * 100
                  isWolfWithNoMoves | possibleMoves (wolf b) b == [] = -100000
                                    | otherwise = 0

getIndexOfBestNodeInTree :: GameTree -> Int
getIndexOfBestNodeInTree (Node t b []) = -1
getIndexOfBestNodeInTree (Node t b treeList) = head ( filter ((== maximum tab) . (tab !!)) [0..] )
    where
    tab = (map (\y -> rateTree y 0) treeList)


rateTree :: GameTree -> Int -> Int
rateTree (Node t b treeList) n  | length treeList /= 0 && t == WolfTurn = maximum tab
                                   | length treeList /= 0 && t == SheepTurn = minimum tab
                                   | otherwise = rateNode (Node t b treeList) n
                                    where
                                    tab = (map (\y -> rateTree y (n+1)) treeList)

getOptimalMove :: Board -> Board
getOptimalMove b =
    let x = generateNode b WolfTurn 0 (generateBoard b WolfTurn)
    in
        if (getIndexOfBestNodeInTree x) /= (-1)
        then
            brd((tab x) !! (getIndexOfBestNodeInTree x))
        else
            b
