module BoardGenerate where
    import System.Random
    import Data.List
    import Tools
    
    generateCountRandom :: Int -> Int -> IO[Int]
    generateCountRandom n m = do
        childs <- randomRIO (2, div (n * m) 10 :: Int)
        obstCount <- randomRIO (1, div (n * m) 10 :: Int) 
        robotsCount <- randomRIO (2, div (n * m) 20 :: Int)
        return [childs, obstCount, robotsCount]
    
    squareGet :: Int -> Int -> Int
    squareGet childsTemp square | product >= childsTemp = square
                                | otherwise = squareGet childsTemp (square + 1)
                                where
                                    product = square * square
    
    hAndwCorralGet :: Int -> Int -> [Int]
    hAndwCorralGet childsTemp square = if ((square - 1) * square)  >= childsTemp then [square - 1, square] else [square, square] 

    coordCorralGet :: Int -> Int -> Int -> Int -> [Int]
    coordCorralGet hc wc h w = [rc0, cc0, rc0 + hc + 1, cc0 + wc + 1]
                            where
                                g = mkStdGen (hc * wc)
                                (rc0, f) = randomR (0, h - hc - 2) g
                                (cc0, _) = randomR (0, w - wc - 2) f

    boardGenerateAll :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] 
    boardGenerateAll h w childrensCount obsCount robotsCount rc0 cc0 rc1 cc1 random = let
        withCorral = boardWithCorral h h w rc0 cc0 rc1 cc1
        withChildrens = boardWithChildrens withCorral h w rc0 cc0 rc1 cc1 childrensCount random
        withDirt = boardWithDirt withChildrens h h w rc0 cc0 rc1 cc1 (random + 3)
        withObstacles = boardWithObstacles withDirt h w rc0 cc0 rc1 cc1 obsCount (random * 7)
        withRobots = boardWithRobots withObstacles h w rc0 cc0 rc1 cc1 robotsCount (random - 8)
        in withRobots
    
    boardWithRobots :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    boardWithRobots board h w rc0 cc0 rc1 cc1 count random = let
        availableBox = availableBoxWithoutCorralGet board h h w w rc0 cc0 rc1 cc1  
        g = mkStdGen random
        positions = positionsGet availableBox count g
        positionsOrder = sort_ positions
        withRobotsGenerate = boardWithRobots1 board h h w positionsOrder (length positionsOrder) (length positionsOrder)
        in withRobotsGenerate

    boardWithRobots1 :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardWithRobots1 board h0 h w positions p0 p    | h0 == 0 = []
                                                    | otherwise = let
                                                        row = rowWithRobots1 board (h - h0) w w positions p0 p
                                                        p1 = jumpGet positions (h - h0 + 1) p0 p
                                                        in row : boardWithRobots1 board (h0 - 1) h w positions p1 p
                                                    
    rowWithRobots1 :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]
    rowWithRobots1 board r w0 w positions p0 p  | w0 == 0 = []
                                                | p0 == 0 = (board!!r!!c) : rowWithRobots1 board r (w0 - 1) w positions 0 p
                                                | robot == [r,c] = 5 : rowWithRobots1 board r (w0 - 1) w positions (p0 - 1) p
                                                | otherwise = (board!!r!!c) : rowWithRobots1 board r (w0 - 1) w positions p0 p
                                                where
                                                    c = w - w0
                                                    robot = positions!!(p - p0)


    boardWithObstacles :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    boardWithObstacles board h w rc0 cc0 rc1 cc1 count random = let
        availableBox = availableBoxWithoutCorralGet board h h w w rc0 cc0 rc1 cc1
        g = mkStdGen (random * count)
        positions = positionsGet availableBox count g
        positionsOrder = sort_ positions
        withObstaclesGenerate = boardWithObstacles1 board h h w positionsOrder (length positionsOrder) (length positionsOrder)
        in withObstaclesGenerate
    
    boardWithObstacles1 :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardWithObstacles1 board h0 h w positions p0 p | h0 == 0 = []
                                                    | otherwise = let
                                                        row = rowWithObstacles1 board (h - h0) w w positions p0 p
                                                        p1 = jumpGet positions (h - h0 + 1) p0 p
                                                        in row : boardWithObstacles1 board (h0 - 1) h w positions p1 p
                                                    
    rowWithObstacles1 :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]
    rowWithObstacles1 board r w0 w positions p0 p   | w0 == 0 = []
                                                    | p0 == 0 = (board!!r!!c) : rowWithObstacles1 board r (w0 - 1) w positions 0 p
                                                    | obstacle == [r,c] = 4 : rowWithObstacles1 board r (w0 - 1) w positions (p0 - 1) p
                                                    | otherwise = (board!!r!!c) : rowWithObstacles1 board r (w0 - 1) w positions p0 p
                                                    where
                                                        c = w - w0
                                                        obstacle = positions!!(p - p0)

    boardWithDirt :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    boardWithDirt board h0 h w rc0 cc0 rc1 cc1 random   | h0 == 0 = []
                                                        | otherwise = let
                                                            g = mkStdGen random
                                                            row = rowWithDirt board h0 h w w rc0 cc0 rc1 cc1 g
                                                            in row : boardWithDirt board (h0 - 1) h w rc0 cc0 rc1 cc1 (random + 3)

    rowWithDirt :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> StdGen -> [Int]
    rowWithDirt board h0 h w0 w rc0 cc0 rc1 cc1 g   | w0 == 0 = []
                                                    | otherwise = let
                                                        n = (r - 1) >= 0 && ((board!!(r - 1)!!c) == 2)
                                                        s = (r + 1) < h && ((board!!(r + 1)!!c) == 2)
                                                        w_ = (c - 1) >= 0 && ((board!!r!!(c - 1)) == 2)
                                                        e = (c + 1) < w && ((board!!r!!(c + 1)) == 2)
                                                        corralOutside = (r <= rc0) || (r >= rc1) || (c <= cc0) || (c >= cc1)
                                                        boxClean = (board!!r!!c) == 0
                                                        (dirt1, q) = randomR (True, False) g
                                                        (dirt2, f) = randomR (True, False) q
                                                        box = if boxClean && corralOutside && dirt1 && dirt2 && (n || s || w_ || e) then 3 else board!!r!!c
                                                        in box : rowWithDirt board h0 h (w0 - 1) w rc0 cc0 rc1 cc1 f
                                                    where
                                                        r = h - h0
                                                        c = w - w0


    
    boardWithChildrens :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    boardWithChildrens board h w rc0 cc0 rc1 cc1 count random = let
        availableBox = availableBoxGet board h h w w rc0 cc0 rc1 cc1
        g = mkStdGen random

        positions0 = positionsGet availableBox count g
        positions1 = childrensInCorralGet positions0 (length positions0) (length positions0) rc0 cc0 rc1 cc1
        positions2 = childrensInCorralGen (length positions1) rc0 cc0 rc1 cc1

        positionsOrder = sort_ ((positions0 \\ positions1) ++ positions2)
        withChildrensGenerate = boardWithChildrens1 board h h w positionsOrder (length positionsOrder) (length positionsOrder)
        in withChildrensGenerate
    
    
    childrensInCorralGet :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    childrensInCorralGet childrens p0 p rc0 cc0 rc1 cc1 | p0 == 0 = []
                                                        | inside = (childrens!!(p - p0)) : childrensInCorralGet childrens (p0 - 1) p rc0 cc0 rc1 cc1
                                                        | otherwise = childrensInCorralGet childrens (p0 - 1) p rc0 cc0 rc1 cc1
                                                        where
                                                            inside = (head (childrens!!(p - p0)) > rc0) && (head (childrens!!(p - p0)) < rc1) && ((childrens!!(p - p0)!!1) > cc0) && ((childrens!!(p - p0)!!1) < cc1) 

    childrensInCorralGen :: Int -> Int -> Int -> Int -> Int -> [[Int]]
    childrensInCorralGen count rc0 cc0 rc1 cc1 = let
        rcMiddle = div (rc0 + rc1) 2
        ccMiddle = div (cc0 + cc1) 2
        in if count > 0 then [rcMiddle, ccMiddle] : childrensInCorralGen1 (count - 1) rcMiddle ccMiddle rc0 cc0 rc1 cc1 1 else []
    
    childrensInCorralGen1 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    childrensInCorralGen1 count rMiddle cMiddle rc0 cc0 rc1 cc1 add   | count == 0 = []
                                                                        | otherwise = let
                                                                            northRange = (rMiddle - add) > rc0 && (rMiddle - add) < rc1 && cMiddle > cc0 && cMiddle < cc1
                                                                            sourthRange = (rMiddle + add) > rc0 && (rMiddle + add) < rc1 && cMiddle > cc0 && cMiddle < cc1
                                                                            westRange = rMiddle > rc0 && rMiddle < rc1 && (cMiddle - add) > cc0 && (cMiddle - add) < cc1
                                                                            eastRange = rMiddle > rc0 && rMiddle < rc1 && (cMiddle + add) > cc0 && (cMiddle + add) < cc1
                                                                            
                                                                            e = [[rMiddle, cMiddle + add] | eastRange]

                                                                            bool2 = (count - length e) > 0 && westRange
                                                                            w = [[rMiddle, cMiddle - add] | bool2]

                                                                            bool3 = (count - length e - length w) > 0 && sourthRange && eastRange
                                                                            se = [[rMiddle + add, cMiddle + add] | bool3]

                                                                            bool4 = (count - length e - length w - length se) > 0 && sourthRange && westRange
                                                                            sw = [[rMiddle + add, cMiddle - add] | bool4]

                                                                            bool5 = (count - length e - length w - length se - length sw) > 0 && sourthRange
                                                                            s = [[rMiddle, cMiddle + add] | bool5]

                                                                            bool6 = (count - length e - length w - length se - length sw - length s) > 0 && northRange && westRange
                                                                            nw = [[rMiddle - add, cMiddle - add] | bool6]

                                                                            bool7 = (count - length e - length w - length se - length sw - length s - length nw) > 0 && northRange && eastRange
                                                                            ne = [[rMiddle - add, cMiddle + add] | bool7]

                                                                            bool8 =  (count - length e - length w - length se - length sw - length s - length nw - length ne) > 0 && northRange
                                                                            n = [[rMiddle - add, cMiddle] | bool8]

                                                                            count1 = count - length e - length w - length se - length sw - length s - length nw - length ne - length n

                                                                            in n ++ s ++ e ++ w ++ ne ++ nw ++ se ++ sw ++ childrensInCorralGen1 count1 rMiddle cMiddle rc0 cc0 rc1 cc1 (add + 1)


    boardWithCorral :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    boardWithCorral h0 h w rc0 cc0 rc1 cc1  | h0 == 0 = []
                                            | otherwise = rowWithCorral (h - h0) w w rc0 cc0 rc1 cc1 : boardWithCorral (h0 - 1) h w rc0 cc0 rc1 cc1
    
    rowWithCorral :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
    rowWithCorral r0 w0 w rc0 cc0 rc1 cc1   | w0 == 0 = []
                                            | (r0 == rc0 || (r0 == rc1)) && ((w - w0) > cc0) && ((w - w0) < cc1) = 1 : rowWithCorral r0 (w0 - 1) w rc0 cc0 rc1 cc1 
                                            | ((w - w0) == cc0 || ((w - w0) == cc1)) && (r0 > rc0) && (r0 < rc1) = 1 : rowWithCorral r0 (w0 - 1) w rc0 cc0 rc1 cc1 
                                            | otherwise = 0 : rowWithCorral r0 (w0 - 1) w rc0 cc0 rc1 cc1

    positionsGet :: [[Int]] -> Int -> StdGen -> [[Int]]
    positionsGet boxs count g   | null boxs || (count == 0) = []
                                | otherwise = let
                                    (i, f) = randomR (0, length boxs - 1) g 
                                    in (boxs!!i) : positionsGet (delete (boxs!!i) boxs) (count - 1) f

    availableBoxGet :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    availableBoxGet board h0 h w0 w rc0 cc0 rc1 cc1 | h0 == 0 = []
                                                    | w0 == 0 = availableBoxGet board (h0 - 1) h w w rc0 cc0 rc1 cc1 
                                                    | availableBox = [r, c] : availableBoxGet board h0 h (w0 - 1) w rc0 cc0 rc1 cc1
                                                    | otherwise = availableBoxGet board h0 h (w0 - 1) w rc0 cc0 rc1 cc1
                                                    where
                                                        r = h - h0
                                                        c = w - w0
                                                        availableBox = (board!!r!!c == 0) && ([r,c] /= [rc0, cc0]) && ([r,c] /= [rc0, cc1]) && ([r,c] /= [rc1, cc0]) && ([r,c] /= [rc1, cc1]) 

    availableBoxWithoutCorralGet :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    availableBoxWithoutCorralGet board h0 h w0 w rc0 cc0 rc1 cc1    | h0 == 0 = []
                                                                    | w0 == 0 = availableBoxWithoutCorralGet board (h0 - 1) h w w rc0 cc0 rc1 cc1 
                                                                    | availableBox = [r, c] : availableBoxWithoutCorralGet board h0 h (w0 - 1) w rc0 cc0 rc1 cc1
                                                                    | otherwise = availableBoxWithoutCorralGet board h0 h (w0 - 1) w rc0 cc0 rc1 cc1
                                                                    where
                                                                        r = h - h0
                                                                        c = w - w0
                                                                        availableBox = (board!!r!!c == 0) && ((r < rc0) || (r > rc1)) && ((c < cc0) || (c > cc1)) 

    boardWithChildrensMove :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> [[Int]]
    boardWithChildrensMove board h w rc0 cc0 rc1 cc1 childrensPositions childrensMove = let
        childrensAll = childrensAllGet board h h w w 
        g = mkStdGen childrensMove
        childrensToMove = positionsGet childrensPositions childrensMove g

        h1 = mkStdGen (childrensMove * childrensMove)
        childrensNewMoveNewDir = newChildrensPositionsGet board h w rc0 cc0 rc1 cc1 childrensToMove (length childrensToMove) (length childrensToMove) h1
        childrensNewMove = childrensNewMoveGet childrensNewMoveNewDir (length childrensNewMoveNewDir) (length childrensNewMoveNewDir)
        obstaclesNewPositions = obstaclesNewPositionsEvaluate board h h w w childrensNewMoveNewDir (length childrensNewMoveNewDir)

        childrensOrder = sort_ ((childrensAll \\ childrensToMove) ++ childrensNewMove)
        obstaclesOrder = sort_ obstaclesNewPositions

        withMovemment = boardWithMovemment board h h w childrensOrder (length childrensOrder) (length childrensOrder) obstaclesOrder (length obstaclesOrder) (length obstaclesOrder)
        withDirt = boardWhitDirtAfterMove withMovemment h w rc0 cc0 rc1 cc1 childrensNewMove (length childrensNewMove) h1
        in withDirt
    
    boardWhitDirtAfterMove :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> StdGen -> [[Int]]
    boardWhitDirtAfterMove board h w rc0 cc0 rc1 cc1 childrensToMove p g = let
        dirtPositions = putDirt board 1 h 1 w rc0 cc0 rc1 cc1 childrensToMove p g
        dirtOrder = sort_ dirtPositions
        withDirt = boardWithDirtAfterMove1 board h h w dirtOrder (length dirtOrder) (length dirtOrder)
        in withDirt

    boardWithDirtAfterMove1 :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardWithDirtAfterMove1 board h0 h w dirt p0 p  | h0 == 0 = []
                                                    | otherwise = let
                                                        row = rowWithDirtAfterMove1 board (h - h0) w w dirt p0 p
                                                        p1 = jumpGet dirt (h - h0 + 1) p0 p
                                                        in row : boardWithDirtAfterMove1 board (h0 - 1) h w dirt p1 p

    rowWithDirtAfterMove1 :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]                               
    rowWithDirtAfterMove1 board r w0 w dirts p0 p   | w0 == 0 = []
                                                    | not emptyDirt && ([r,c_] == dirt) = 3 : rowWithDirtAfterMove1 board r (w0 - 1) w dirts (p0 - 1) p                                
                                                    | otherwise = box : rowWithDirtAfterMove1 board r (w0 - 1) w dirts p0 p 
                                                    where 
                                                        c_ = w - w0
                                                        emptyDirt = p0 == 0
                                                        dirt = dirts!!(p - p0)
                                                        box = board!!r!!c_                            


    putDirt :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> StdGen -> [[Int]]
    putDirt board r0 h c0 w rc0 cc0 rc1 cc1 childrensToMove p g | r0 >= h = []
                                                                | c0 >= w = putDirt board (r0 + 3) h 1 w rc0 cc0 rc1 cc1 childrensToMove p g
                                                                | otherwise = let
                                                                    positions = [[r0,c0], [r0 - 1, c0 - 1], [r0 - 1, c0], [r0 - 1, c0 + 1], [r0, c0 + 1], [r0 + 1, c0 + 1], [r0 + 1, c0], [r0 + 1, c0 - 1], [r0, c0 - 1]]
                                                                    childrensCountMove = childrensToMoveFind childrensToMove p positions 0

                                                                    (dirtCount, f)  | childrensCountMove == 1 = randomR (0, 1) g
                                                                                    | childrensCountMove == 2 = randomR (0, 3) g
                                                                                    | childrensCountMove >= 3 = randomR (0, 6) g
                                                                                    | otherwise = (0, g)

                                                                    newPosDirt = putDirt1 board h w rc0 cc0 rc1 cc1 positions 0 dirtCount
                                                                    in newPosDirt ++ putDirt board r0 h (c0 + 3) w rc0 cc0 rc1 cc1 childrensToMove p f
    
    putDirt1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    putDirt1 board h w rc0 cc0 rc1 cc1 positions i dirtCount    | (i == 9) || (dirtCount == 0) = []
                                                                | availableBox = [r, c] : putDirt1 board h w rc0 cc0 rc1 cc1 positions (i + 1) (dirtCount - 1)
                                                                | otherwise =  putDirt1 board h w rc0 cc0 rc1 cc1 positions (i + 1) dirtCount
                                                                where
                                                                    r = head (positions!!i)
                                                                    c = positions!!i!!1
                                                                    isRange = (r >= 0) && (r < h) && (c >= 0) && (c < w)
                                                                    corralOut = (r <= rc0) || (r >= rc1) || (c <= cc0) || (c >= cc1)
                                                                    availableBox = isRange && corralOut && (board!!r!!c == 0)

    childrensToMoveFind :: [[Int]] -> Int -> [[Int]] -> Int -> Int
    childrensToMoveFind childrensToMove p positions i   | i == 9 = 0
                                                        | exist = 1 + childrensToMoveFind childrensToMove p positions (i + 1)
                                                        | otherwise = childrensToMoveFind childrensToMove p positions (i + 1)
                                                        where
                                                            exist = check childrensToMove (positions!!i) p p
    
    obstaclesNewPositionsEvaluate :: [[Int]] -> Int -> Int -> Int -> Int -> [[[Int]]] -> Int -> [[Int]]
    obstaclesNewPositionsEvaluate board h0 h w0 w newMoveNewDir p   | h0 == 0 = []
                                                                    | w0 == 0 = obstaclesNewPositionsEvaluate board (h0 - 1) h w w newMoveNewDir p
                                                                    | obstacle && not (null children) = let
                                                                        c = head children
                                                                        d = children!!1
                                                                        newPos = obstaclesNewPosGet board h w (head c + head d) (c!!1 + d!!1) d
                                                                        in newPos :  obstaclesNewPositionsEvaluate board h0 h (w0 - 1) w newMoveNewDir p
                                                                    | obstacle && null children = [r,c] : obstaclesNewPositionsEvaluate board h0 h (w0 - 1) w newMoveNewDir p
                                                                    | otherwise = obstaclesNewPositionsEvaluate board h0 h (w0 - 1) w newMoveNewDir p
                                                                    where
                                                                        r = h - h0
                                                                        c = w - w0
                                                                        obstacle = board!!r!!c == 4
                                                                        children = childrenExist [r,c] newMoveNewDir p p
    
    childrenExist :: [Int] -> [[[Int]]] -> Int -> Int -> [[Int]]
    childrenExist pos newMoveNewDir p0 p    | p0 == 0 = []
                                            | pos == head children = children
                                            | otherwise = childrenExist pos newMoveNewDir (p0 - 1) p
                                            where
                                                children = newMoveNewDir!!(p - p0)

    boardWithMovemment :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardWithMovemment board h0 h w childrensNewMove c0 c obstaclesPut o0 o | h0 == 0 = []
                                                                            | otherwise = let
                                                                                row = rowWithMovemment board (h - h0) w w childrensNewMove c0 c obstaclesPut o0 o
                                                                                c1 = jumpGet childrensNewMove (h - h0 + 1) c0 c
                                                                                o1 = jumpGet obstaclesPut (h - h0 + 1) o0 o
                                                                                in row : boardWithMovemment board (h0 - 1) h w childrensNewMove c1 c obstaclesPut o1 o

    rowWithMovemment :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]                               
    rowWithMovemment board r w0 w childrensNewMove c0 c obstaclesPut o0 o   | w0 == 0 = []
                                                                            | not emptyChildrens && ([r,c_] == children) = 2 : rowWithMovemment board r (w0 - 1) w childrensNewMove (c0 - 1) c obstaclesPut o0 o                                
                                                                            | not emptyObs && ([r,c_] == obs) = 4 : rowWithMovemment board r (w0 - 1) w childrensNewMove c0 c obstaclesPut (o0 - 1) o
                                                                            | (box == 2) || (box == 4) = 0 : rowWithMovemment board r (w0 - 1) w childrensNewMove c0 c obstaclesPut o0 o                                
                                                                            | otherwise = box : rowWithMovemment board r (w0 - 1) w childrensNewMove c0 c obstaclesPut o0 o    
                                                                            where 
                                                                                c_ = w - w0
                                                                                emptyChildrens = c0 == 0
                                                                                children = childrensNewMove!!(c - c0)
                                                                                emptyObs = o0 == 0
                                                                                obs = obstaclesPut!!(o - o0)
                                                                                box = board!!r!!c_                            

    newChildrensPositionsGet :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> StdGen -> [[[Int]]]
    newChildrensPositionsGet board h w rc0 cc0 rc1 cc1 childrensToMove p0 p g   | p0 == 0 = []
                                                                                | otherwise = let
                                                                                                
                                                                                    availableNorth = availableChildMove board h w rc0 cc0 rc1 cc1 (r - 1) c [-1, 0] 
                                                                                    availableSouth = availableChildMove board h w rc0 cc0 rc1 cc1 (r + 1) c [1, 0]
                                                                                    availableEast =  availableChildMove board h w rc0 cc0 rc1 cc1 r (c + 1) [0, 1]
                                                                                    availableWest =  availableChildMove board h w rc0 cc0 rc1 cc1 r (c - 1) [0, -1]
                                                                                    availableNW =    availableChildMove board h w rc0 cc0 rc1 cc1 (r - 1) (c - 1) [-1, -1] 
                                                                                    availableNE =    availableChildMove board h w rc0 cc0 rc1 cc1 (r - 1) (c + 1) [-1, 1]
                                                                                    availableSW =  availableChildMove board h w rc0 cc0 rc1 cc1 (r + 1) (c - 1) [1, -1]
                                                                                    availableSE =  availableChildMove board h w rc0 cc0 rc1 cc1 (r + 1) (c + 1) [1, 1]

                                                                                    dirN = [[r - 1, c] | availableNorth]
                                                                                    n = [[-1, 0] | availableNorth]

                                                                                    dirNW = [[r - 1, c - 1] | availableNW]
                                                                                    nw = [[-1, -1] | availableNW]

                                                                                    dirNE = [[r - 1, c + 1] | availableNE]
                                                                                    ne = [[-1, 1] | availableNE]

                                                                                    dirW = [[r, c - 1] | availableWest]
                                                                                    w_ = [[0, -1] | availableWest]

                                                                                    dirSW = [[r + 1, c - 1] | availableSW]
                                                                                    sw = [[1, -1] | availableSW]

                                                                                    dirS = [[r + 1, c] | availableSouth]
                                                                                    s = [[1, 0] | availableSouth]

                                                                                    dirSE = [[r + 1, c + 1] | availableSE]
                                                                                    se = [[1, 1] | availableSE]

                                                                                    dirE = [[r, c + 1] | availableEast]
                                                                                    e = [[0, 1] | availableEast]

                                                                                    directions = dirN ++ dirNW ++ dirNE ++ dirW ++ dirSW ++ dirS ++ dirSE ++ dirE
                                                                                    walk = n ++ nw ++ ne ++ w_ ++ sw ++ s ++ se ++ e

                                                                                    (pos, f) = randomR (0, length directions - 1) g

                                                                                    newDir = if not (null directions) then directions!!pos else [r,c]
                                                                                    newWalk = if not (null directions) then walk!!pos else [0,0]

                                                                                    in [newDir, newWalk] : newChildrensPositionsGet board h w rc0 cc0 rc1 cc1 childrensToMove (p0 - 1) p f 
                                                                                where
                                                                                    r = head (childrensToMove!!(p - p0))
                                                                                    c = childrensToMove!!(p - p0)!!1

    
    availableChildMove :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> Bool
    availableChildMove board h w rc0 cc0 rc1 cc1 r c dir    | (r < 0) || (r >= h) || (c < 0) || (c >= w) = False
                                                            | (board!!r!!c == 0) && not corralBounded = True
                                                            | board!!r!!c == 4 = availableChildMove board h w rc0 cc0 rc1 cc1 (r + head dir)  (c + dir!!1) dir
                                                            | otherwise = False
                                                            where
                                                                corralBounded = ([r,c] == [rc0, cc0]) || ([r,c] == [rc0, cc1]) || ([r,c] == [rc1, cc0]) || ([r,c] == [rc1, cc1])

    obstaclesNewPosGet :: [[Int]] -> Int -> Int -> Int -> Int -> [Int] -> [Int]
    obstaclesNewPosGet board h w r c dir    | r < 0 || r >= h || c < 0 || c >= w = []
                                            | board!!r!!c == 0 = [r, c]
                                            | board!!r!!c == 4 = obstaclesNewPosGet board h w (r + head dir)  (c + dir!!1) dir
                                            | otherwise = []


    environmentClean::[[Int]] -> Int -> Int -> Bool
    environmentClean board h w = let
        dirtsPositions = dirtsGet board h h w w
        count = length dirtsPositions
        in div (count * 100) (h * w) < 40