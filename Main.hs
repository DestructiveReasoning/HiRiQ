import Data.Maybe
import System.Exit
import System.IO
import System.Posix.Process
import System.Posix.Signals
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data Direction = MoveLeft|MoveRight|MoveUp|MoveDown

fill = convertAttributes[Reverse]
initColors = do
    initPair (Pair 1) (Color 17) defaultBackground
    initPair (Pair 2) (Color 60) defaultBackground

color_white = Pair 2
color_black = Pair 1

endSmall = [2,26,29]
endBig = [5,12,19]
begHorizontal = [0,3,6,13,20,27,30]
endHorizontal = [2,5,12,19,26,29,32]
begVertical = [6,7,0,1,2,11,12]
endVertical = [20,21,30,31,32,25,26]

scale = 3
cell = replicate (scale * 2) ' '

--replaceElement (Eq a) => Integer -> [a] -> a -> [a]
--replaceElement index newVal list = take index  list ++ newVal:drop (index + 1) list

invertTriplet :: [Int] -> [Bool] -> [Bool]
invertTriplet [] list = list
invertTriplet (i:is) list = invertTriplet is $ take i list ++ (not (list !! i)):drop (i + 1) list

cursmove :: Window -> Int -> (Int,Int) -> Direction -> IO (Int,Int,Int)
cursmove w index (y,x) dir = do
    case dir of
        MoveUp -> do
            if index `elem` begVertical then return(index,y,x)
            else do 
--                wMove w (y-1,x)
                if index <= 5 then return (index - 3,y-scale,x)
                else if index `elem` [8,9,10] then return (index - 5, y-scale, x)
                else if index `elem` [13,14..26] then return (index - 7, y - scale, x)
                else if index `elem` [27,28,29] then return (index - 5, y - scale, x)
                else return (index - 3, y-scale, x)
        MoveDown -> do
            if index `elem` endVertical then return(index,y,x)
            else do 
--                wMove w (y+1,x)
                if index <= 2 then return (index + 3,y+scale,x)
                else if index <= 5 then return (index + 5,y+scale,x)
                else if index <= 19 then return (index + 7, y+scale,x)
                else if index <= 24 then return (index + 5, y+scale,x)
                else return (index+3,y+scale,x)
        MoveLeft -> do
            if index `elem` begHorizontal then return(index,y,x)
            else do 
--                wMove w (y,x-1)
                return (index - 1,y,x-(scale * 2))
        MoveRight -> do
            if index `elem` endHorizontal then return(index,y,x)
            else do 
--                wMove w (y,x+1)
                return (index + 1,y,x+(scale * 2))

drawBoard :: Window -> [Bool] -> Int -> (Int,Int) -> IO()
drawBoard w board index (y,x) = do
    refresh
--    if index >= (length board) then do return()
    if index >= (length board) then do return()
    else do
        (col, row) <- scrSize
        if (board !! index) == True then do
            wAttrSet w (fill, color_white)
        else do
            wAttrSet w (fill, color_black)
        drawCell 0 
--        mvWAddStr w y x cell
        if index `elem` endBig then drawBoard w board (index + 1) (y + scale, row `div` 2 - 6*scale)
        else if index `elem` endSmall then drawBoard w board (index + 1) (y + scale, row `div` 2 - 2*scale)
        else drawBoard w board (index + 1) (y, x + 2*scale)
        where drawCell n = do
                if (n >= scale) then do return ()
                else do
                    mvWAddStr w (y+n) x cell
                    drawCell (n+1)

applyMove :: Direction -> Int -> [Bool] -> IO [Bool]
applyMove dir index board = do
    case dir of
        MoveUp -> do
            if index < 8 then return board
            else if index `elem` [11,12,13,14,18,19] then return board
            else if index `elem` [8,9,10] then 
                    do
                        if ((board !! index) == (board !! (index - 5))) && ((board !! index) == (board !! (index - 8))) then return board
                        else if ((board !! index) == (board !! (index - 8))) && ((board !! index) /= (board !! (index - 5))) then return board
                        else return $ invertTriplet [index, index - 5, index - 8] board
            else if index `elem` [30,31,32] then 
                    do
                        if ((board !! index) == (board !! (index - 3))) && ((board !! index) == (board !! (index - 8))) then return board
                        else if ((board !! index) == (board !! (index - 8))) && ((board !! index) /= (board !! (index - 3))) then return board
                        else return $ invertTriplet [index, index - 3, index - 8] board
            else if index `elem` [27,28,29] then
                    do 
                        if ((board !! index) == (board !! (index - 5))) && ((board !! index) == (board !! (index - 12))) then return board
                        else if ((board !! index) == (board !! (index - 12))) && ((board !! index) /= (board !! (index - 5))) then return board
                        else return $ invertTriplet [index, index - 5, index - 12] board
            else if index `elem` [15,16,17] then
                    do 
                        if ((board !! index) == (board !! (index - 7))) && ((board !! index) == (board !! (index - 12))) then return board
                        else if ((board !! index) == (board !! (index - 12))) && ((board !! index) /= (board !! (index - 7))) then return board
                        else return $ invertTriplet [index, index - 7, index - 12] board
            else do
                        if ((board !! index) == (board !! (index - 7))) && ((board !! index) == (board !! (index - 14))) then return board
                        else if ((board !! index) == (board !! (index - 14))) && ((board !! index) /= (board !! (index - 7))) then return board
                        else return $ invertTriplet [index, index - 14, index - 7] board
        MoveDown -> do
            if index < 0 then return board
            else if index `elem` [13,14,20,21,25,26,18,19,27,28,29,30,31,32] then return board
            else if index `elem` [0,1,2] then 
                    do
                        if ((board !! index) == (board !! (index + 3))) && ((board !! index) == (board !! (index + 8))) then return board
                        else if ((board !! index) == (board !! (index + 8))) && ((board !! index) /= (board !! (index + 3))) then return board
                        else return $ invertTriplet [index, index + 3, index + 8] board
            else if index `elem` [22,23,24] then 
                    do
                        if ((board !! index) == (board !! (index + 5))) && ((board !! index) == (board !! (index + 8))) then return board
                        else if ((board !! index) == (board !! (index + 8))) && ((board !! index) /= (board !! (index + 5))) then return board
                        else return $ invertTriplet [index, index + 5, index + 8] board
            else if index `elem` [3,4,5] then
                    do 
                        if ((board !! index) == (board !! (index + 5))) && ((board !! index) == (board !! (index + 12))) then return board
                        else if ((board !! index) == (board !! (index + 12))) && ((board !! index) /= (board !! (index + 5))) then return board
                        else return $ invertTriplet [index, index + 5, index + 12] board
            else if index `elem` [15,16,17] then
                    do 
                        if ((board !! index) == (board !! (index + 7))) && ((board !! index) == (board !! (index + 12))) then return board
                        else if ((board !! index) == (board !! (index + 12))) && ((board !! index) /= (board !! (index + 7))) then return board
                        else return $ invertTriplet [index, index + 7, index + 12] board
            else do
                        if ((board !! index) == (board !! (index + 7))) && ((board !! index) == (board !! (index + 14))) then return board
                        else if ((board !! index) == (board !! (index + 14))) && ((board !! index) /= (board !! (index + 7))) then return board
                        else return $ invertTriplet [index, index + 14, index + 7] board
        MoveLeft -> do
            if index `elem` [0,1,3,4,27,28,30,31,7,14,21,6,13,20] then return board
            else
                    do
                        if ((board !! index) == (board !! (index - 1))) && ((board !! index) == (board !! (index - 2))) then return board
                        else if ((board !! index) == (board !! (index - 2))) && ((board !! index) /= (board !! (index - 1))) then return board
                        else return $ invertTriplet [index,index-1,index-2] board
        MoveRight -> do
            if index `elem` [1,2,4,5,28,29,31,32,11,18,25,12,19,26] then return board
            else
                    do
                        if ((board !! index) == (board !! (index + 1))) && ((board !! index) == (board !! (index + 2))) then return board
                        else if ((board !! index) == (board !! (index + 2))) && ((board !! index) /= (board !! (index + 1))) then return board
                        else return $ invertTriplet [index,index+1,index+2] board

play :: Window -> Int -> (Int, Int) -> [Bool] -> IO()
play w index (cursY, cursX) board = do
--    file <- openFile "res" AppendMode
    (y,x) <- scrSize
    let centerX = y `div` 2
        centerY = x `div` 2
--    wclear w
    drawBoard w board 0 (centerX - 4*scale,centerY - 2*scale)
--    update
--    refresh
    mvWAddStr w 0 0 $ "Index: " ++ (show index)
--    putStrLn $ "Index': " ++ (show index)
--    hPutStrLn file (show index)
    wMove w cursY cursX
    refresh
    update
    c <- getCh
--    hClose file
    if c ==  KeyChar 'q' then return ()
    else if c == KeyChar 'h' then do 
        (i,newY, newX) <- cursmove w index (cursY, cursX) MoveLeft
        play w i (newY,newX) board
    else if c == KeyChar 'j' then do 
        (i, newY, newX) <- cursmove w index (cursY, cursX) MoveDown
--        mvWAddStr w 0 10 $ "Index': " ++ (show i)
--        putStrLn $ "Index': " ++ (show i)
        play w i (newY,newX) board
    else if c == KeyChar 'k' then do 
        (i, newY, newX) <- cursmove w index (cursY, cursX) MoveUp
--        mvWAddStr w 0 10 $ "Index': " ++ (show i)
--        putStrLn $ "Index': " ++ (show i)
        play w i (newY,newX) board
    else if c == KeyChar 'l' then do 
        (i, newY, newX) <- cursmove w index (cursY, cursX) MoveRight
        play w i (newY,newX) board
    else if c == KeyChar ' ' then do
        play w index (cursY,cursX) ((take index board) ++ [not (board !! index)] ++ (drop (index+1) board))
    else if c == KeyUp then do
        newArray <- applyMove MoveUp index board
        play w index (cursY, cursX) newArray
    else if c == KeyDown then do
        newArray <- applyMove MoveDown index board
        play w index (cursY,cursX) newArray
    else if c == KeyLeft then do
        newArray <- applyMove MoveLeft index board
        play w index (cursY,cursX) newArray
    else if c == KeyRight then do
        newArray <- applyMove MoveRight index board
        play w index (cursY,cursX) newArray
    else play w index (cursY, cursX) board

resize :: IO()
resize = do
    (y,x) <- resizeui
    resizeTerminal y x

cleanStop :: IO()
cleanStop = do
    endWin
    exitImmediately ExitSuccess

main = do
--    cursSet CursorInvisible
    initCurses
    initColors
    echo False
    w <- initScr
    (y,x) <- scrSize
    cBreak True
    noDelay w True
    keypad w True
    let sigwinch = fromJust cursesSigWinch
    installHandler sigwinch (Catch resize) Nothing
    installHandler keyboardSignal (Catch cleanStop) Nothing
    refresh
    update
    play w 0 (y `div` 2 - 4*scale, x `div` 2 - 2*scale) (replicate 33 False)
    endWin
    putStrLn $ show (y,x)
