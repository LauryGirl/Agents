module DeductiveReasoning where
    import Tools    ( boardWithChildrens1,
                    corralPutChildrens,
                    jumpGetRobot,
                    robotsWithChildsGet,
                    robotsWithoutChildsGet,
                    sort_ )
    import Data.List ( (\\), delete )

    play :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[[Int]]] 
    play board h w rc0 cc0 rc1 cc1 childsDown = let
        agentWithChildrens = robotsWithChildsGet board h h w w childsDown 
        agentWithoutChildrens = robotsWithoutChildsGet board h h w w childsDown
        
        board_ = boardWithChildrens1 board h h w childsDown (length childsDown) (length childsDown)
        newAgents : newDown : _ = agentsSolve board_ h w rc0 cc0 rc1 cc1 agentWithChildrens agentWithoutChildrens

        boardResult = withAgentActivities board_ h w newAgents
        
        in [boardResult, newDown]
        
    
    agentsSolve :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[[Int]]]
    agentsSolve board h w rc0 cc0 rc1 cc1 agentsChildrens agentsAlone = let
        corralPut = corralPutChildrens board (length agentsChildrens) (rc0 + 1) (cc0 + 1) cc0 rc1 cc1 
        
        dr = [0, 0, 1, 1, 1, -1, -1, -1]
        dc = [1,-1, 1,-1, 0, -1,  1,  0]
        agent1Works = homeworksPriorityChilds board h w rc0 cc0 rc1 cc1 agentsChildrens (length agentsChildrens) (length agentsChildrens) corralPut dr dc
        agent2Works = homeworksPriority board h w rc0 cc0 rc1 cc1 agentsAlone (length agentsAlone) (length agentsAlone) dr dc

        newAgentsNewDown = homeworksGet (agent1Works ++ agent2Works) h w

        in newAgentsNewDown
    
    homeworksGet :: [[[Int]]] -> Int -> Int -> [[[Int]]]
    homeworksGet agentsWorks h w = let
        worksSort = agentsSortByWorks agentsWorks
        workByAgent = onlyWorkByAgent worksSort (length worksSort)  (length worksSort) []
        newAgent : newDown : _ = newWorkTake workByAgent h w (length workByAgent) (length workByAgent) [] []
        in [newAgent, sort_ newDown]
    
    newWorkTake :: [[[Int]]] -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> [[[Int]]]
    newWorkTake workByAgent h w p0 p newWorks newDown   | p0 == 0 = [newWorks, newDown]
                                                        | null down = newWorkTake workByAgent h w (p0 - 1) p (newWork: newWorks) newDown
                                                        | otherwise = newWorkTake workByAgent h w (p0 - 1) p (newWork: newWorks) (down : newDown)
                                                        where
                                                            work = workByAgent!!(p - p0)
                                                            task = head (work!!1)
                                                            newr = work!!1!!1
                                                            newc = work!!1!!2
                                                            newWork | task <= ((h * w) + 5) = [6, newr, newc]
                                                                    | otherwise = [5, newr, newc]
                                                            down    | task == 0 = [newr, newc]
                                                                    | otherwise = []
    
    onlyWorkByAgent :: [[[Int]]] -> Int -> Int -> [[[Int]]] -> [[[Int]]]
    onlyWorkByAgent worksSort p0 p new_ | p0 == 0 = new_
                                        | agentOrPosTaked = onlyWorkByAgent worksSort (p0 - 1) p new_
                                        | otherwise = onlyWorkByAgent worksSort (p0 - 1) p (work : new_)
                                        where
                                            work = worksSort!!(p - p0)
                                            agentOrPosTaked = agentAndNewPosTaked new_ work (length new_) (length new_)
    
    agentAndNewPosTaked :: [[[Int]]] -> [[Int]] -> Int -> Int -> Bool
    agentAndNewPosTaked works work p0 p | p0 == 0 = False
                                        | otherwise = (agent1 == agent2) || (newPos1 == newPos2) || agentAndNewPosTaked works work (p0 - 1) p
                                        where
                                            work__ = works!!(p - p0)
                                            agent1 = head work
                                            agent2 = head work__
                                            newPos1 = [work!!1!!1, work!!1!!2]
                                            newPos2 = [work__!!1!!1, work__!!1!!2]

    agentsSortByWorks :: [[[Int]]] -> [[[Int]]]
    agentsSortByWorks [] = [] 
    agentsSortByWorks agentsWorks = agents ++ agentsSortByWorks (agentsWorks \\ agents)
                                    where
                                        task = minimum (elemsGet agentsWorks 1 0 (length agentsWorks) (length agentsWorks))
                                        agents = tupleGet agentsWorks task (length agentsWorks) (length agentsWorks)
    
    elemsGet :: [[[Int]]] -> Int -> Int -> Int -> Int -> [Int]
    elemsGet agentsWork i j p0 p    | p0 == 0 = []
                                    | otherwise = task : elemsGet agentsWork i j (p0 - 1) p
                                    where
                                        task = agentsWork!!(p - p0)!!i!!j
    
    tupleGet :: [[[Int]]] -> Int -> Int -> Int -> [[[Int]]]
    tupleGet agentsWorks task p0 p  | p0 == 0 = []
                                    | t == task = agent : tupleGet agentsWorks task (p0 - 1) p
                                    | otherwise = tupleGet agentsWorks task (p0 - 1) p
                                    where
                                        t = head (agentsWorks!!(p - p0)!!1)
                                        agent = agentsWorks!!(p - p0)

    homeworksPriorityChilds :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]] -> [Int] -> [Int] -> [[[Int]]]
    homeworksPriorityChilds board h w rc0 cc0 rc1 cc1 agents p0 p corralPut dr dc   | p0 == 0 = []
                                                                                    | otherwise = let
                                                                                        work = homeworksPriorityChild1 board h w rc0 cc0 rc1 cc1 agent corralPut dr dc 0
                                                                                        in work ++ homeworksPriorityChilds board h w rc0 cc0 rc1 cc1 agents (p0 - 1) p corralPut dr dc
                                                                                    where 
                                                                                        agent = agents!!(p - p0)

    homeworksPriorityChild1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [[Int]] -> [Int] -> [Int] -> Int -> [[[Int]]]
    homeworksPriorityChild1 board h w rc0 cc0 rc1 cc1 agent corralPut dr dc i   | i == 8 = []
                                                                                | not move = homeworksPriorityChild1 board h w rc0 cc0 rc1 cc1 agent corralPut dr dc (i + 1)
                                                                                | otherwise = let
                                                                                    task = taskAgentChildGet board r h c w corralPut rc0 cc0 rc1 cc1 
                                                                                    newCorralPut = if task == 0 then delete [r,c] corralPut else corralPut
                                                                                    allMoves = homeworksPriorityChild2 board h w rc0 cc0 rc1 cc1 agent corralPut dr dc (i + 1) [r,c]

                                                                                    in [[agent, [task, r, c]]] ++ allMoves ++ homeworksPriorityChild1 board h w rc0 cc0 rc1 cc1 agent newCorralPut dr dc (i + 1)
                                                                                where
                                                                                    r = head agent + dr!!i
                                                                                    c = agent!!1 + dc!!i
                                                                                    move = moveChildOutCorralRule board r h c w rc0 cc0 rc1 cc1 || moveInCorralRule board r h c w rc0 cc0 rc1 cc1

    homeworksPriorityChild2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [[Int]] -> [Int] -> [Int] -> Int -> [Int] -> [[[Int]]]
    homeworksPriorityChild2 board h w rc0 cc0 rc1 cc1 agent corralPut dr dc i pos0  | i == 8 = []
                                                                                    | not move = homeworksPriorityChild2 board h w rc0 cc0 rc1 cc1 agent corralPut dr dc (i + 1) pos0
                                                                                    | otherwise = let
                                                                                        task = taskAgentChildGet board r h c w corralPut rc0 cc0 rc1 cc1 
                                                                                        newCorralPut = if task == 0 then delete [r,c] corralPut else corralPut

                                                                                        in [agent, [task, r, c]] : homeworksPriorityChild2 board h w rc0 cc0 rc1 cc1 agent newCorralPut dr dc (i + 1) pos0
                                                                                    where
                                                                                        r = head pos0 + dr!!i
                                                                                        c = pos0!!1 + dc!!i
                                                                                        move = moveChildOutCorralRule board r h c w rc0 cc0 rc1 cc1 || moveInCorralRule board r h c w rc0 cc0 rc1 cc1

    homeworksPriority :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [Int] -> [Int] -> [[[Int]]]
    homeworksPriority board h w rc0 cc0 rc1 cc1 agents p0 p dr dc   | p0 == 0 = []
                                                                    | otherwise = let
                                                                        work = if agentInsideCorral then homeworksPriorityOutside board h w rc0 cc0 rc1 cc1 agent dr dc 0 else homeworksPriority1 board h w rc0 cc0 rc1 cc1 agent dr dc 0
                                                                        in work ++ homeworksPriority board h w rc0 cc0 rc1 cc1 agents (p0 - 1) p dr dc
                                                                        where 
                                                                            agent = agents!!(p - p0)
                                                                            r = head agent
                                                                            c = agent!!1
                                                                            agentInsideCorral = corralInside  r c rc0 cc0 rc1 cc1

    homeworksPriorityOutside :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> [[[Int]]]
    homeworksPriorityOutside board h w rc0 cc0 rc1 cc1 agent dr dc i    | i == 8 = []
                                                                        | not move = homeworksPriorityOutside board h w rc0 cc0 rc1 cc1 agent dr dc (i + 1)
                                                                        | otherwise = let
                                                                            task = taskAgentAloneCorralGet board r h c w rc0 cc0 rc1 cc1 

                                                                            in [agent, [task, r, c]] : homeworksPriorityOutside board h w rc0 cc0 rc1 cc1 agent dr dc (i + 1)
                                                                        where
                                                                            r = head agent + dr!!i
                                                                            c = agent!!1 + dc!!i
                                                                            move = moveOutCorralRule board r h c w rc0 cc0 rc1 cc1 || moveInCorralRule board r h c w rc0 cc0 rc1 cc1


    homeworksPriority1 :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> [[[Int]]]
    homeworksPriority1 board h w rc0 cc0 rc1 cc1 agent dr dc i  | i == 8 = []
                                                                | not move = homeworksPriority1 board h w rc0 cc0 rc1 cc1 agent dr dc (i + 1)
                                                                | otherwise = let
                                                                    task = taskAgentAloneGet board r h c w rc0 cc0 rc1 cc1 

                                                                    in [agent, [task, r, c]] : homeworksPriority1 board h w rc0 cc0 rc1 cc1 agent dr dc (i + 1)
                                                                where
                                                                    r = head agent + dr!!i
                                                                    c = agent!!1 + dc!!i
                                                                    move = moveOutCorralRule board r h c w rc0 cc0 rc1 cc1 || moveInCorralRule board r h c w rc0 cc0 rc1 cc1


    taskAgentChildGet :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> Int -> Int -> Int 
    taskAgentChildGet board r h c w corralPut rc0 cc0 rc1 cc1   | not (null corralPut) &&  (head corralPut == [r,c]) = 0
                                                                | corralInside r c rc0 cc0 rc1 cc1 = 1 + abs (r - rPut) + abs (c - cPut)
                                                                | corralBounded r c rc0 cc0 rc1 cc1 && emptyInsideCorral board r h c w rc0 cc0 rc1 cc1  = 2 + abs (r - rPut) + abs (c - cPut)
                                                                | clearRule board r h c w = 3 + abs (r - rPut) + abs (c - cPut)
                                                                | otherwise  = let
                                                                    dist1   | inRange rc0 h cc0 w && emptyInsideCorral board rc0 h cc0 w rc0 cc0 rc1 cc1 = 4 + abs (r - rc0) + abs (c - cc0)  
                                                                            | otherwise = (h * w)        
                                                                    dist2   | inRange rc0 h cc1 w && emptyInsideCorral board rc0 h cc1 w rc0 cc0 rc1 cc1 = 4 + abs (r - rc0) + abs (c - cc1)  
                                                                            | otherwise = (h * w)        
                                                                    dist3   | inRange rc1 h cc0 w && emptyInsideCorral board rc1 h cc0 w rc0 cc0 rc1 cc1= 4 + abs (r - rc1) + abs (c - cc0)  
                                                                            | otherwise = (h * w)        
                                                                    dist4   | inRange rc1 h cc1 w  && emptyInsideCorral board rc1 h cc1 w rc0 cc0 rc1 cc1= 4 + abs (r - rc1) + abs (c - cc1)  
                                                                            | otherwise = (h * w)                                    
                                                                    in minimum [dist1, dist2, dist3, dist4] + abs (r - rPut) + abs (c - cPut)  
                                                                where 
                                                                    rPut = if null corralPut then r else head (head corralPut)
                                                                    cPut = if null corralPut then c else head corralPut!!1                                                        

    lookRadar :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> [Int]
    lookRadar board h w rc0 cc0 rc1 cc1 pos dr dc i     | i == 8 = []
                                                        | not move = lookRadar board h w rc0 cc0 rc1 cc1 pos dr dc (i + 1)
                                                        | otherwise = let
                                                            task = taskAgentAloneRadarGet board r h c w rc0 cc0 rc1 cc1
                                                            in task : lookRadar board h w rc0 cc0 rc1 cc1 pos dr dc (i + 1)
                                                        where
                                                            r = head pos + dr!!i
                                                            c = pos!!1 + dc!!i
                                                            move = moveOutCorralRule board r h c w rc0 cc0 rc1 cc1 || moveInCorralRule board r h c w rc0 cc0 rc1 cc1

    taskAgentAloneRadarGet :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int                                     
    taskAgentAloneRadarGet board r h c w rc0 cc0 rc1 cc1    | upChildRule board r h c w rc0 cc0 rc1 cc1 = 5
                                                            | clearRule board r h c w = 6
                                                            | otherwise = 7 + objectMoreLess board r h c w rc0 cc0 rc1 cc1


    taskAgentAloneGet :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int                                     
    taskAgentAloneGet board r h c w rc0 cc0 rc1 cc1 | upChildRule board r h c w rc0 cc0 rc1 cc1 = (h * w) + 5
                                                    | clearRule board r h c w = (h * w) + 6
                                                    | otherwise = let
                                                        dr = [0, 0, 1, 1, 1, -1, -1, -1]
                                                        dc = [1,-1, 1,-1, 0, -1,  1,  0]
                                                        newTask = minimum (lookRadar board h w rc0 cc0 rc1 cc1 [r,c] dr dc 0)
                                                        in (h * w) + 7 + newTask

    taskAgentAloneCorralGet :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int                                     
    taskAgentAloneCorralGet board r h c w rc0 cc0 rc1 cc1   | corralOutside r c rc0 cc0 rc1 cc1 = (h * w) + 9
                                                            | otherwise  = let
                                                                pos00 = if emptyInsideCorral board rc0 h cc0 w rc0 cc0 rc1 cc1 then (h * w) + 10 + abs ( r - rc0) + abs (c - cc0)
                                                                                                                            else (2 * h * w) + 10
                                                                pos01 = if emptyInsideCorral board rc0 h cc1 w rc0 cc0 rc1 cc1 then (h * w) + 10 + abs ( r - rc0) + abs (c - cc1)
                                                                                                                            else (2 * h * w) + 10
                                                                pos02 = if emptyInsideCorral board rc1 h cc0 w rc0 cc0 rc1 cc1 then (h * w) + 10 + abs ( r - rc1) + abs (c - cc0)
                                                                                                                           else (2 * h * w) + 10 
                                                                pos03 = if emptyInsideCorral board rc1 h cc1 w rc0 cc0 rc1 cc1 then (h * w) + 10 + abs ( r - rc1) + abs (c - cc1)
                                                                                                                            else (2 * h * w) + 10
                                                                in minimum [pos00, pos01, pos02, pos03]
                                                   
    objectMoreLess :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int 
    objectMoreLess board r h c w rc0 cc0 rc1 cc1 = let
        objectsDistance = objectsDistanceSolve  board r c h h w w rc0 cc0 rc1 cc1
        in if null objectsDistance then 0 else minimum objectsDistance

    objectsDistanceSolve :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
    objectsDistanceSolve board r c h0 h w0 w rc0 cc0 rc1 cc1    | h0 == 0 = []
                                                                | w0 == 0 = objectsDistanceSolve board r c (h0 - 1) h w w rc0 cc0 rc1 cc1
                                                                | objectBox = (abs (r - r_) + abs (c - c_)) : objectsDistanceSolve board r c h0 h (w0 - 1) w rc0 cc0 rc1 cc1
                                                                | otherwise = objectsDistanceSolve board r c h0 h (w0 - 1) w rc0 cc0 rc1 cc1
                                                                where
                                                                    r_ = h - h0
                                                                    c_ = w - w0
                                                                    box = board!!r_!!c_
                                                                    corralOut = corralOutside r_ c_ rc0 cc0 rc1 cc1
                                                                    objectBox = corralOut && ((box == 2) || (box == 3))

    inRange :: Int -> Int -> Int -> Int -> Bool 
    inRange r h c w = (r >= 0) && (r < h) && (c >= 0) && (c < w) 

    emptyInsideCorral :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
    emptyInsideCorral board r h c w rc0 cc0 rc1 cc1 | [r,c] == [rc0, cc0] = inRange (r + 1) h (c + 1) w && (((board!!(r + 1)!!(c + 1)) == 0) || ((board!!(r + 1)!!(c + 1)) == 5) || ((board!!(r + 1)!!(c + 1)) == 6))
                                                    | [r,c] == [rc0, cc1] = inRange (r + 1) h (c - 1) w && (((board!!(r + 1)!!(c - 1)) == 0) || ((board!!(r + 1)!!(c - 1)) == 5) || ((board!!(r + 1)!!(c - 1)) == 6))
                                                    | [r,c] == [rc1, cc0] = inRange (r - 1) h (c + 1) w && (((board!!(r - 1)!!(c + 1)) == 0) || ((board!!(r - 1)!!(c + 1)) == 5) || ((board!!(r - 1)!!(c + 1)) == 6))
                                                    | otherwise  = inRange (r - 1) h (c - 1) w && (((board!!(r - 1)!!(c - 1)) == 0) || ((board!!(r - 1)!!(c - 1)) == 5) || ((board!!(r - 1)!!(c - 1)) == 6))

    corralInside :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
    corralInside r c rc0 cc0 rc1 cc1 = (r > rc0) && (r < rc1) && (c > cc0) && (c < cc1)

    corralOutside :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
    corralOutside r c rc0 cc0 rc1 cc1 = not (corralInside r c rc0 cc0 rc1 cc1)

    corralBounded :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
    corralBounded r c rc0 cc0 rc1 cc1 = ([r,c] == [rc0, cc0]) || ([r,c] == [rc0, cc1]) || ([r,c] == [rc1, cc0]) || ([r,c] == [rc1, cc1])

    clearRule :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
    clearRule board r h c w = inRange r h c w && ((board!!r!!c) == 3)

    upChildRule :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
    upChildRule board r h c w rc0 cc0 rc1 cc1 = inRange r h c w && corralOutside r c rc0 cc0 rc1 cc1 && ((board!!r!!c) == 2)

    downChildRule :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int-> [Int] -> Bool
    downChildRule board r h c w rc0 cc0 rc1 cc1 posf = inRange r h c w && corralInside r c rc0 cc0 rc1 cc1 && ((board!!r!!c) == 0) && ([r,c] == posf)

    moveOutCorralRule :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
    moveOutCorralRule board r h c w rc0 cc0 rc1 cc1 = inRange r h c w && corralOutside r c rc0 cc0 rc1 cc1 && (((board!!r!!c) /= 1) && ((board!!r!!c) /= 4))
                                            
    moveInCorralRule :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
    moveInCorralRule board r h c w rc0 cc0 rc1 cc1 = inRange r h c w && corralInside r c rc0 cc0 rc1 cc1 && (((board!!r!!c) /= 1) && ((board!!r!!c) /= 2))

    moveChildOutCorralRule :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
    moveChildOutCorralRule board r h c w rc0 cc0 rc1 cc1 = inRange r h c w && corralOutside r c rc0 cc0 rc1 cc1 && (((board!!r!!c) /= 1) && ((board!!r!!c) /= 4) && ((board!!r!!c) /= 2))
                                            
    withAgentActivities :: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
    withAgentActivities board h w agents = let
        board1 = boardCleanedRobots board h h w
        agentsSort = robotsSort agents
        board2 = boardWithAgentActivities board1 h h w agentsSort (length agentsSort) (length agentsSort)
        in board2
    

    boardWithAgentActivities :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardWithAgentActivities board h0 h w agents p0 p   | h0 == 0 = []
                                                        | otherwise = let
                                                            row = rowWithAgentActivities board (h - h0) w w agents p0 p
                                                            p1 = jumpGetRobot agents (h - h0 + 1) p0 p
                                                            in row : boardWithAgentActivities board (h0 - 1) h w agents p1 p

    rowWithAgentActivities :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]                               
    rowWithAgentActivities board r w0 w agents p0 p | w0 == 0 = []
                                                    | not emptyAgent && ([r,c_] == pos) = value : rowWithAgentActivities board r (w0 - 1) w agents (p0 - 1) p                                
                                                    | otherwise = box : rowWithAgentActivities board r (w0 - 1) w agents p0 p 
                                                    where 
                                                        c_ = w - w0
                                                        emptyAgent = p0 == 0
                                                        value = head (agents!!(p - p0))
                                                        pos = [agents!!(p - p0)!!1, agents!!(p - p0)!!2]
                                                        box = board!!r!!c_                            


    robotsSort :: [[Int]] -> [[Int]]
    robotsSort [] = []
    robotsSort xs = [v, r, c] : robotsSort (delete [v, r, c] xs)
                    where
                        l = length xs
                        r = minimum (rowRobotsGet xs l l)
                        c = minimum (columnRobotsGet xs r l l)
                        v = valueRobotGet xs r c l l
    
    valueRobotGet :: [[Int]] -> Int -> Int -> Int -> Int -> Int
    valueRobotGet xs r c p0 p   | p0 == 0 = 0
                                | box == [r,c] = value
                                | otherwise = valueRobotGet xs r c (p0 - 1) p
                                where
                                    r_ = xs!!(p - p0)!!1
                                    c_ = xs!!(p - p0)!!2
                                    value = head (xs!!(p - p0))
                                    box = [r_, c_]
    
    columnRobotsGet ::  [[Int]] -> Int -> Int -> Int -> [Int]
    columnRobotsGet xs r p0 p   | p0 == 0 = []
                                | r_ == r = c : columnRobotsGet xs r (p0 - 1) p
                                | otherwise = columnRobotsGet xs r (p0 - 1) p
                                where
                                    r_ = xs!!(p - p0)!!1
                                    c = xs!!(p - p0)!!2
    
    rowRobotsGet :: [[Int]] -> Int -> Int -> [Int]
    rowRobotsGet xs p0 p    | p0 == 0 = []
                            | otherwise = r : rowRobotsGet xs (p0 - 1) p
                            where 
                                r = xs!!(p - p0)!!1

    boardCleanedRobots :: [[Int]] -> Int -> Int -> Int -> [[Int]]
    boardCleanedRobots board h0 h w | h0 == 0 = []
                                    | otherwise = let
                                        row = rowCleanedRobots board (h - h0) w w 
                                        in row : boardCleanedRobots board (h0 - 1) h w
    
    rowCleanedRobots :: [[Int]] -> Int -> Int -> Int -> [Int]
    rowCleanedRobots board r w0 w   | w0 == 0 = []
                                    | box == 5 || box == 6 = 0 : rowCleanedRobots board r (w0 - 1) w
                                    | otherwise = box : rowCleanedRobots board r (w0 - 1) w
                                    where
                                        c = w - w0
                                        box = board!!r!!c