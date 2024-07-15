{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.Gtk
import Data.IORef
import Graphics.UI.Gtk.Gdk.GC(Color)
import System.Random (randomRIO)
import Control.Monad (replicateM)

type Grid = [[Bool]]

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew :: IO Window
    
    set window [windowTitle := ("Minesweeper" :: String), 
                containerBorderWidth := 20, 
                windowDefaultWidth := 600, 
                windowDefaultHeight := 600]
    
    levelBox <- dialogNew
    let lightGreen = Color 36959 61166 36959
    widgetModifyBg levelBox StateNormal lightGreen
    widgetModifyBg levelBox StateActive lightGreen
    widgetModifyBase levelBox StateNormal lightGreen
    widgetModifyBase levelBox StateActive lightGreen

    let beginnerID = ResponseUser 1
    let intermediateID = ResponseUser 2
    let expertID = ResponseUser 3


    dialogAddButton levelBox ("Beginner" :: String) (beginnerID)
    dialogAddButton levelBox ("Intermediate" :: String) (intermediateID)
    dialogAddButton levelBox ("Expert" :: String) (expertID)

    response <- dialogRun levelBox
    let (rows, cols,numMines) = case response of
                         response | response == beginnerID  -> (8,8,10)
                         response | response == intermediateID -> (16,16,40)
                         response | response == expertID -> (30,16,99)
                         _              -> (8,8,10) 
    widgetDestroy levelBox

    grid <- tableNew rows cols True :: IO Table
    buttons <- mapM (\(i, j) -> do
                        btn <- buttonNewWithLabel ("🐥" :: String)
                        widgetModifyBg btn StateNormal (Color 65535 65535 52428) 
                        widgetModifyBg btn StateActive (Color 34695 52851 60074) 
                        widgetModifyBase btn StateNormal (Color 65535 65535 52428) 
                        widgetModifyBase btn StateActive (Color 34695 52851 60074) 
                         
                        tableAttachDefaults grid btn i (i+1) j (j+1)
                        return btn) [(i, j) | i <- [0..rows-1], j <- [0..cols-1]]
    
    let buttonRefs = [((i, j), btn) | ((i, j), btn) <- zip [(i, j) | i <- [0..rows-1], j <- [0..cols-1]] buttons]

    mines <- newIORef (replicate rows (replicate cols False))
    mapM_ (\((i, j), btn) -> on btn buttonActivated (tileClicked mines buttonRefs (i, j) rows cols)) buttonRefs
    initialGrid <- newIORef (replicate rows (replicate cols False))
    finalGrid <- placeMines (replicate rows (replicate cols False)) numMines
    writeIORef initialGrid finalGrid

    mapM_ (\((i, j), btn) -> on btn buttonActivated (tileClicked rows cols initialGrid buttonRefs (i, j))) buttonRefs
    containerAdd window grid
    on window objectDestroy mainQuit
    widgetShowAll window
    mainGUI


placeMines :: Grid -> Int -> IO Grid
placeMines grid numMines = do
    let rows = length grid
    let cols = length (head grid)
    minePositions <-replicateM numMines $ randomRIO ((0, 0), (rows - 1, cols - 1))
    return $ foldl (\g (r, c) -> placeMine g r c) grid minePositions

placeMine :: Grid -> Int -> Int -> Grid
placeMine grid row col = take row grid ++ [take col (grid !! row) ++ [True] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

tileClicked :: Int -> Int -> IORef [[Bool]] -> [((Int, Int), Button)] -> (Int, Int) -> IO ()
tileClicked rows cols mines buttons (i, j) = do
    mineField <- readIORef mines
    if mineField !! i !! j
    then putStrLn "Game Over!"
    else do
        let neighbors = [(i+di, j+dj) | di <- [-1..1], dj <- [-1..1], i+di >= 0, i+di < rows, j+dj >= 0, j+dj < cols]
        let mineCount = length $ filter id [mineField !! x !! y | (x, y) <- neighbors]
        let Just btn = lookup (i, j) buttons
        set btn [buttonLabel := show mineCount]
        let lightPink = Color 65535 46774 49544
        widgetModifyBg btn StateNormal lightPink
        widgetModifyBg btn StateActive lightPink
        widgetModifyBase btn StateNormal lightPink
        widgetModifyBase btn StateActive lightPink
