module Tools where
    import Data.List ( delete )

    rowList :: [[Int]] -> Int -> Int -> [Int]
    rowList xs p0 p | p0 == 0 = []
                    | otherwise = head (xs!!(p - p0)) : rowList xs (p0 - 1) p

    columnList :: [[Int]] -> Int -> Int -> Int -> [Int]
    columnList xs r p0 p    | p0 == 0 = []
                            | head (xs!!(p - p0)) == r = (xs!!(p - p0)!!1) : columnList xs r (p0 - 1) p
                            | otherwise =  columnList xs r (p0 - 1) p

    sort_ :: [[Int]] -> [[Int]]
    sort_ [] = []
    sort_ xs = x : sort_ (delete x xs)
            where
                r = minimum (rowList xs (length xs) (length xs))
                c = minimum (columnList xs r (length xs) (length xs))
                x = [r, c]
    
    jumpGet :: [[Int]] -> Int -> Int -> Int -> Int
    jumpGet positions r p0 p    | p0 == 0 = 0
                                | otherwise = p1
                                where
                                    p1 = if head (positions!!(p - p0)) >= r then p0 else jumpGet positions r (p0 - 1) p

    jumpGetRobot :: [[Int]] -> Int -> Int -> Int -> Int
    jumpGetRobot positions r p0 p   | p0 == 0 = 0
                                    | otherwise = p1
                                    where
                                        p1 = if (positions!!(p - p0)!!1) >= r then p0 else jumpGetRobot positions r (p0 - 1) p

    draw :: [[Int]] -> Int -> Int -> IO()
    draw board h w = do
        drawWithColor board h h w
    
    drawWithColor :: [[Int]] -> Int -> Int -> Int -> IO()
    drawWithColor board h0 h w  | h0 == 0 = return()
                                | otherwise = do
                                    let line = drawLineWithColor board (h - h0) w w
                                    putStrLn line
                                    putStrLn ""
                                    drawWithColor board (h0 - 1) h w
    
    drawLineWithColor :: [[Int]] -> Int -> Int -> Int -> String 
    drawLineWithColor board r w0 w  | w0 == 0 = ""
                                    | box == 0 = "\ESC[90mx" ++ "  " ++ drawLineWithColor board r (w0 - 1) w
                                    | box == 1 = "\ESC[96mx" ++ "  " ++ drawLineWithColor board r (w0 - 1) w 
                                    | box == 2 = "\ESC[92m*" ++ "  " ++ drawLineWithColor board r (w0 - 1) w
                                    | box == 3 = "\ESC[93mx" ++ "  " ++ drawLineWithColor board r (w0 - 1) w
                                    | box == 4 = "\ESC[91mx" ++ "  " ++ drawLineWithColor board r (w0 - 1) w
                                    | box == 5 = "\ESC[94m0" ++ "  " ++ drawLineWithColor board r (w0 - 1) w
                                    | otherwise= "\ESC[92m0" ++ "  " ++ drawLineWithColor board r (w0 - 1) w
                                    where
                                        box = board!!r!!(w - w0)
    robotsWithChildsGet :: [[Int]] -> Int -> Int -> Int ->  Int -> [[Int]] -> [[Int]]
    robotsWithChildsGet board h0 h w0 w childsDown  | h0 == 0 = []
                                                    | w0 == 0 = robotsWithChildsGet board (h0 - 1) h w w childsDown 
                                                    | value == 6 && not inChildsPos = box : robotsWithChildsGet board h0 h (w0 - 1) w childsDown
                                                    | otherwise = robotsWithChildsGet board h0 h (w0 - 1) w childsDown
                                                    where
                                                        r = h - h0
                                                        c = w - w0
                                                        value = board!!r!!c
                                                        box = [r,c]
                                                        inChildsPos = check childsDown box (length childsDown) (length childsDown)

    robotsWithoutChildsGet :: [[Int]] -> Int -> Int -> Int ->  Int -> [[Int]] -> [[Int]]
    robotsWithoutChildsGet board h0 h w0 w childsDown   | h0 == 0 = []
                                                        | w0 == 0 = robotsWithoutChildsGet board (h0 - 1) h w w childsDown
                                                        | value == 5 = box : robotsWithoutChildsGet board h0 h (w0 - 1) w  childsDown
                                                        | value == 6 && inChildsPos = box : robotsWithoutChildsGet board h0 h (w0 - 1) w  childsDown
                                                        | otherwise = robotsWithoutChildsGet board h0 h (w0 - 1) w  childsDown
                                                        where
                                                            r = h - h0
                                                            c = w - w0
                                                            value = board!!r!!c
                                                            box = [r,c]
                                                            inChildsPos = check childsDown box (length childsDown) (length childsDown)


    childrensGet :: [[Int]] -> Int -> Int -> Int ->  Int -> Int ->  Int -> Int ->  Int -> [[Int]]
    childrensGet board h0 h w0 w rc0 cc0 rc1 cc1    | h0 == 0 = []
                                                    | w0 == 0 = childrensGet board (h0 - 1) h w w rc0 cc0 rc1 cc1
                                                    | value == 2 && corralOutside = box : childrensGet board h0 h (w0 - 1) w rc0 cc0 rc1 cc1
                                                    | otherwise = childrensGet board h0 h (w0 - 1) w rc0 cc0 rc1 cc1
                                                    where
                                                        r = h - h0
                                                        c = w - w0
                                                        value = board!!r!!c
                                                        box = [r,c]
                                                        corralOutside = not ((r > rc0) && (r < rc1) && (c > cc0) && (c < cc1))

    childrensAllGet :: [[Int]] -> Int -> Int -> Int ->  Int -> [[Int]]
    childrensAllGet board h0 h w0 w | h0 == 0 = []
                                    | w0 == 0 = childrensAllGet board (h0 - 1) h w w
                                    | value == 2 = box : childrensAllGet board h0 h (w0 - 1) w
                                    | otherwise = childrensAllGet board h0 h (w0 - 1) w
                                    where
                                        r = h - h0
                                        c = w - w0
                                        value = board!!r!!c
                                        box = [r,c]

    
    childrensNewMoveGet :: [[[Int]]] -> Int -> Int -> [[Int]]
    childrensNewMoveGet newMoveNewDir p0 p  | p0 == 0 = []
                                            | otherwise = head (newMoveNewDir!!(p - p0)) : childrensNewMoveGet newMoveNewDir (p0 - 1) p

    check::[[Int]] -> [Int] -> Int -> Int -> Bool
    check list_ item 0 m = False
    check list_ item m0 m =  (valueList == item) || check list_ item (m0 - 1) m
                            where
                                valueList = list_!!(m - m0)

    bla_ :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
    bla_ board h0 h w0 w    | h0 == 0 = []
                            | w0 == 0 = bla_ board (h0 - 1) h w w
                            | value == 6 || value == 5 = [value, r,c] : bla_ board h0 h (w0 - 1) w
                            | otherwise = bla_ board h0 h (w0 - 1) w
                            where
                                r = h - h0
                                c = w - w0
                                value = board!!r!!c

    corralPutChildrens :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    corralPutChildrens board count r0 c0 cc0 rc1 cc1    | count == 0 = []
                                                        | r0 == rc1 = []
                                                        | c0 == cc1 = corralPutChildrens board count (r0 + 1) cc0 cc0 rc1 cc1 
                                                        | availableBox = [r0,c0] : corralPutChildrens board (count - 1) r0 (c0 + 1) cc0 rc1 cc1
                                                        | otherwise = corralPutChildrens board count r0 (c0 + 1) cc0 rc1 cc1
                                                        where
                                                            availableBox = (board!!r0!!c0 /= 1) && (board!!r0!!c0 /= 2)
                                                                            
    boardWithChildrens1 :: [[Int]] -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
    boardWithChildrens1 board h0 h w positions p0 p | h0 == 0 = []
                                                    | otherwise = let
                                                        row = rowWithChildrens1 board h0 h w w positions p0 p
                                                        p1 = jumpGet positions (h - h0 + 1) p0 p
                                                        in row : boardWithChildrens1 board (h0 - 1) h w positions p1 p
    
    rowWithChildrens1 :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]] -> Int -> Int -> [Int]
    rowWithChildrens1 board h0 h w0 w positions p0 p    | w0 == 0 = []
                                                        | p0 == 0 = (board!!r!!c) : rowWithChildrens1 board h0 h (w0 - 1) w positions 0 p
                                                        | otherwise = let
                                                            box = if [r,c] == child then -1 else board!!r!!c
                                                            in if box == -1 then 2 : rowWithChildrens1 board h0 h (w0 - 1) w positions (p0 - 1) p
                                                                            else box : rowWithChildrens1 board h0 h (w0 - 1) w positions p0 p
                                                        where
                                                            r = h - h0
                                                            c = w - w0
                                                            child = positions!!(p - p0)  
    
    dirtsGet :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
    dirtsGet board h0 h w0 w    | h0 == 0 = []
                                | w0 == 0 = dirtsGet board (h0 - 1) h w w
                                | box == 3 = [r,c] : dirtsGet board h0 h (w0 - 1) w
                                | otherwise = dirtsGet board h0 h (w0 - 1) w
                                where
                                    r = h - h0
                                    c = w - w0
                                    box = board!!r!!c
