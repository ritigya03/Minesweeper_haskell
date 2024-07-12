{-# LANGUAGE OverloadedStrings #-}
import Graphics.UI.Gtk
import Data.IORef
import Graphics.UI.Gtk.Gdk.GC(Color)





main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew :: IO Window
    
    set window [windowTitle := ("Minesweeper" :: String), 
                containerBorderWidth := 20, 
                windowDefaultWidth := 600, 
                windowDefaultHeight := 600]
    
    levelBox <- dialogNew
    dialogAddButton levelBox ("Beginner" :: String) ResponseYes
    dialogAddButton levelBox ("Intermediate" :: String) ResponseNo
    dialogAddButton levelBox ("Expert" :: String) ResponseAccept

    response <- dialogRun levelBox
    let (rows, cols) = case response of
                         ResponseYes    -> (8,8)
                         ResponseNo     -> (16,16)
                         ResponseAccept -> (30,16)
                         _              -> (8,8) 
    widgetDestroy levelBox

    grid <- tableNew rows cols True :: IO Table
    buttons <- mapM (\(i, j) -> do
                        btn <- buttonNew
                        widgetModifyBg btn StateNormal (Color 65535 65535 52428) 
                        widgetModifyBg btn StateActive (Color 34695 52851 60074) 
                        widgetModifyBase btn StateNormal (Color 65535 65535 52428) 
                        widgetModifyBase btn StateActive (Color 34695 52851 60074) 
                         
                        tableAttachDefaults grid btn i (i+1) j (j+1)
                        return btn) [(i, j) | i <- [0..rows-1], j <- [0..cols-1]]
    
    let buttonRefs = [((i, j), btn) | ((i, j), btn) <- zip [(i, j) | i <- [0..rows-1], j <- [0..cols-1]] buttons]

    mines <- newIORef (replicate rows (replicate cols False))
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
        let lightPink = Color 65535 46774 49544
        widgetModifyBg btn StateNormal lightPink
        widgetModifyBg btn StateActive lightPink
        widgetModifyBase btn StateNormal lightPink
        widgetModifyBase btn StateActive lightPink
