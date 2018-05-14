import Board

data GameTree = Node Turn Board Float

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

-- generateTree :: Board -> Turn -> GameTree
-- generateTree b t | t == WolfTurn =

-------------------------------

-- s = printBoard b
-- putStr s


t = WolfTurn
b = Board (Wolf(0,1)) [Sheep(1,1), Sheep(3,3), Sheep(5,5), Sheep(7,7)]
w = Wolf(0,0)
s = Sheep(1,1)
xs = [Sheep(1,1), Sheep(3,4), Sheep(5,5), Sheep(7,7)]



