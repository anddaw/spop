import Board

data GameTree = Node Turn Board [GameTree] deriving Show

isWolfBehindThreeSheep :: Board -> Bool
isWolfBehindThreeSheep (Board w s) = (countNumberOfSheepBehindWolf w s) >= 3

countNumberOfSheepBehindWolf :: Piece -> [Piece] -> Int
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

permutationsOfAllSheep :: [Piece] -> [Piece]  -> Board -> [[(Int, Int)]]
permutationsOfAllSheep [] _ _ = []
permutationsOfAllSheep (s:xs) sheepList b = (bigPermutationForOneSheep (possibleMoves s b) s sheepList b) ++ permutationsOfAllSheep xs sheepList b
                    where
                    bigPermutationForOneSheep [] _ _ _ = []
                    bigPermutationForOneSheep (newPosition:xp) sheep sheepList b = [(smallPermutationForOneSheep sheep newPosition sheepList b)]
                        ++ (bigPermutationForOneSheep xp sheep sheepList b)
                            where
                                smallPermutationForOneSheep sheep newSheepPos sheepList b = filter (/= (pfield sheep)) (newSheepPos : (map pfield sheepList))

permutationsOfWolf :: Piece -> Board -> [(Int, Int)]
permutationsOfWolf w b = possibleMoves w b

mapujDoBoardaOwce :: Piece -> [[(Int, Int)]] -> [Board]
mapujDoBoardaOwce w s = map (\z -> Board w z) (map (\x ->  map (\y -> Sheep y) x) s)

mapujDoBoardaWilka :: [(Int, Int)] -> [Piece] -> [Board]
mapujDoBoardaWilka w s = map (\z -> Board z s) (map (\x -> Wolf x) w)

oppositeTurn WolfTurn = SheepTurn
oppositeTurn SheepTurn = WolfTurn

generateBoard :: Board -> Turn -> [Board]
generateBoard b t | t == SheepTurn = mapujDoBoardaOwce (wolf b) (permutationsOfAllSheep (sheep b) (sheep b) b)
                  | t == WolfTurn = mapujDoBoardaWilka (permutationsOfWolf (wolf b) b) (sheep b)

generateNode :: Board -> Turn -> Int -> [Board] -> GameTree
generateNode b t _ [] = Node t b []
generateNode b t n listaNextow | n < 1 = Node t b (map (\x->(generateNode x (oppositeTurn t) (n+1) (generateBoard x (oppositeTurn t)))) listaNextow)
                               | otherwise = Node t b []

rateNode :: Floating a => GameTree -> a
rateNode (Node _ b _) = beingBehindThreeSheep + beingFarFromEdges + beingFarFromSheep
            where
                  x = fst (pfield (wolf b))
                  y = snd (pfield (wolf b))
                  beingBehindThreeSheep | isWolfBehindThreeSheep b = 1000
                                        | otherwise  = 0
                  beingFarFromEdges     | x > 0 && x < 7 && y > 0 && y < 7 = 100
                                        | otherwise = 0
                  beingFarFromSheep = countMeanDistanceToAllSheep (wolf b) (sheep b)

rateTree :: (Floating a, Ord a) => GameTree -> a
rateTree (Node t b treeList) | length treeList /= 0 && t == WolfTurn = maximum (map (\y -> rateNode y) treeList)
                             | length treeList /= 0 && t == SheepTurn = minimum (map (\y -> rateNode y) treeList)
                             | otherwise = 0

-------------------------------

t = WolfTurn
b = Board (Wolf(5,6)) [Sheep(1,6), Sheep(3,6), Sheep(5,6), Sheep(7,0)]
s = [Sheep(1,0), Sheep(3,0), Sheep(5,0), Sheep(7,0)]
one = Wolf(5,5)
lista = generateNode b t 0 (generateBoard b t)
p = rateTree lista

node = Node t (Board (Wolf(5,6)) [Sheep(1,6), Sheep(3,6), Sheep(5,6), Sheep(7,0)]) []
x = rateNode node

-- s = printBoard b
-- putStr s



