{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.Gtk
import Data.IORef

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew :: IO Window
    
    set window [windowTitle := ("Minesweeper" :: String), 
                containerBorderWidth := 10, 
                windowDefaultWidth := 400, 
                windowDefaultHeight := 400]

    grid <- tableNew 8 8 True :: IO Table
    buttons <- mapM (\(i, j) -> do
                        btn <- buttonNew
                        tableAttachDefaults grid btn i (i+1) j (j+1)
                        return btn) [(i, j) | i <- [0..7], j <- [0..7]]
    
    let buttonRefs = [((i, j), btn) | ((i, j), btn) <- zip [(i, j) | i <- [0..7], j <- [0..7]] buttons]

    mines <- newIORef (replicate 8 (replicate 8 False))
    mapM_ (\((i, j), btn) -> on btn buttonActivated (tileClicked mines buttonRefs (i, j))) buttonRefs
    containerAdd window grid
    on window objectDestroy mainQuit
    widgetShowAll window
    mainGUI

tileClicked :: IORef [[Bool]] -> [((Int, Int), Button)] -> (Int, Int) -> IO ()
tileClicked mines buttons (i, j) = do
    mineField <- readIORef mines
    if mineField !! i !! j
    then putStrLn "Game Over!"
    else do
        -- Update the button label
        let neighbors = [(i+di, j+dj) | di <- [-1..1], dj <- [-1..1], i+di >= 0, i+di < 10, j+dj >= 0, j+dj < 10]
        let mineCount = length $ filter id [mineField !! x !! y | (x, y) <- neighbors]
        let Just btn = lookup (i, j) buttons
        set btn [buttonLabel := show mineCount]
