import Graphics.UI.Gtk
import Data.IORef
import Graphics.UI.Gtk.Gdk.GC (Color)
import System.Random (randomRIO)
import Control.Monad (replicateM, forM_)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

type Grid = [[Cell]]

data Cell = Mine | Empty | Revealed Int | Flagged | FlaggedMine deriving (Show, Eq)

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
    let (rows, cols, numMines) = case response of
                                   r | r == beginnerID -> (8, 8, 10)
                                   r | r == intermediateID -> (16, 16, 40)
                                   r | r == expertID -> (30, 16, 99)
                                   _ -> (8, 8, 10) 
    widgetDestroy levelBox

    grid <- tableNew rows cols True :: IO Table
    buttons <- mapM (\(i, j) -> do
                        btn <- buttonNewWithLabel ("🐥" :: String)
                        widgetModifyBg btn StateNormal (Color 65535 65535 52428) 
                        widgetModifyBg btn StateActive (Color 65535 46774 49544) 
                        widgetModifyBase btn StateNormal (Color 65535 65535 52428) 
                        widgetModifyBase btn StateActive (Color 65535 46774 49544) 
                        tableAttachDefaults grid btn i (i + 1) j (j + 1)
                        return btn) [(i, j) | i <- [0..rows - 1], j <- [0..cols - 1]]
    
    let buttonRefs = [((i, j), btn) | ((i, j), btn) <- zip [(i, j) | i <- [0..rows - 1], j <- [0..cols - 1]] buttons]

    initialGrid <- newIORef (replicate rows (replicate cols Empty))
    finalGrid <- placeMines (replicate rows (replicate cols Empty)) numMines
    writeIORef initialGrid finalGrid

    mapM_ (\((i, j), btn) -> do
              on btn buttonActivated (tileClicked window rows cols initialGrid buttonRefs (i, j))
              on btn buttonPressEvent $ do
                  button <- eventButton
                  liftIO $ if button == RightButton
                           then flagTile window initialGrid buttonRefs (i, j)
                           else return False
                  return False
          ) buttonRefs
          
    containerAdd window grid
    on window objectDestroy mainQuit
    widgetShowAll window
    mainGUI

flagTile :: Window -> IORef Grid -> [((Int, Int), Button)] -> (Int, Int) -> IO Bool
flagTile window grid buttons (i, j) = do
    currentGrid <- readIORef grid
    let Just btn = lookup (i, j) buttons
    case currentGrid !! i !! j of
        Empty -> do
            writeIORef grid (updateCell currentGrid i j Flagged)
            postGUIAsync $ buttonSetLabel btn ("🚩" :: String)
        Flagged -> do
            writeIORef grid (updateCell currentGrid i j Empty)
            postGUIAsync $ buttonSetLabel btn ("🐥" :: String)
        Mine -> do
            writeIORef grid (updateCell currentGrid i j FlaggedMine)
            postGUIAsync $ buttonSetLabel btn ("🚩" :: String)
        FlaggedMine -> do
            writeIORef grid (updateCell currentGrid i j Mine)
            postGUIAsync $ buttonSetLabel btn ("🐥" :: String)
        _ -> return ()
    return True

updateCell :: Grid -> Int -> Int -> Cell -> Grid
updateCell grid row col cell = 
    take row grid ++ [take col (grid !! row) ++ [cell] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

tileClicked :: Window -> Int -> Int -> IORef Grid -> [((Int, Int), Button)] -> (Int, Int) -> IO ()
tileClicked window rows cols grid buttons (i, j) = do
    currentGrid <- readIORef grid
    case currentGrid !! i !! j of
        Mine -> do
            let Just btn = lookup (i, j) buttons
            postGUIAsync $ buttonSetLabel btn ("💣" :: String)
            postGUIAsync $ putStrLn "Game Over!"
            postGUIAsync $ set window [windowTitle := ("Game Over" :: String)]
            _ <- timeoutAdd (widgetDestroy window >> return False) 1000 
            return ()
        Empty -> do
            let Just btn = lookup (i, j) buttons
            let count = countMines currentGrid i j
            if count == 0
                then buttonSetLabel btn ("🍀" :: String)
                else buttonSetLabel btn (show count)
            let lightBlue = Color 58980 62258 65535
            widgetModifyBg btn StateNormal lightBlue
            widgetModifyBg btn StateActive lightBlue
            widgetModifyBase btn StateNormal lightBlue
            widgetModifyBase btn StateActive lightBlue
            if count == 0
                then revealAdjacentTiles rows cols grid buttons (i, j)
                else return ()
        _ -> return ()

revealAdjacentTiles :: Int -> Int -> IORef Grid -> [((Int, Int), Button)] -> (Int, Int) -> IO ()
revealAdjacentTiles rows cols grid buttons (i, j) = do
    currentGrid <- readIORef grid
    revealedTiles <- newIORef []
    revealTile rows cols grid buttons (i, j) revealedTiles

revealTile :: Int -> Int -> IORef Grid -> [((Int, Int), Button)] -> (Int, Int) -> IORef [(Int, Int)] -> IO ()
revealTile rows cols grid buttons (i, j) revealedTiles = do
    currentGrid <- readIORef grid
    revealed <- readIORef revealedTiles
    if (i, j) `elem` revealed
        then return ()
        else do
            let Just btn = lookup (i, j) buttons
            let count = countMines currentGrid i j
            if count == 0
                then buttonSetLabel btn ("🍀" :: String)
                else buttonSetLabel btn (show count)
            let lightBlue = Color 58980 62258 65535
            widgetModifyBg btn StateNormal lightBlue
            widgetModifyBg btn StateActive lightBlue
            widgetModifyBase btn StateNormal lightBlue
            widgetModifyBase btn StateActive lightBlue
            modifyIORef revealedTiles ((i, j) :)
            if count == 0
                then do
                    let neighbours = getNeighbours currentGrid i j
                    forM_ neighbours $ \(r, c) -> revealTile rows cols grid buttons (r, c) revealedTiles
                else return ()
    return ()

countMines :: Grid -> Int -> Int -> Int
countMines grid row col =
    length . filter (== Mine) $ map (\(r, c) -> grid !! r !! c) (getNeighbours grid row col)

getNeighbours :: Grid -> Int -> Int -> [(Int, Int)]
getNeighbours grid row col =
    let rows = length grid
        cols = length (head grid)
        potentialNeighbours = [(row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
                               (row, col - 1),                 (row, col + 1),
                               (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)]
    in filter (\(r, c) -> r >= 0 && r < rows && c >= 0 && c < cols) potentialNeighbours

placeMines :: Grid -> Int -> IO Grid
placeMines grid numMines = do
    let rows = length grid
    let cols = length (head grid)
    minePositions <- replicateM numMines $ randomRIO ((0, 0), (rows - 1, cols - 1))
    return $ foldl (\g (r, c) -> placeMine g r c) grid minePositions

placeMine :: Grid -> Int -> Int -> Grid
placeMine grid row col =
    take row grid ++ [take col (grid !! row) ++ [Mine] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

