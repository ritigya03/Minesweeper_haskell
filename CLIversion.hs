import System.Random (randomRIO)
import Control.Monad (replicateM)
import Text.Printf (printf)
type Grid = [[Cell]]
data Cell = FlagMine | FlagRevealed | Mine | Empty | Revealed Int deriving (Show, Eq) 


initializeGrid :: Int -> Int -> IO Grid 
initializeGrid rows cols = return $ replicate rows (replicate cols Empty)

placeMines :: Grid -> Int -> IO Grid
placeMines grid numMines = do
    let rows = length grid
    let cols = length (head grid)
    minePositions <- replicateM numMines $ randomRIO ((0, 0), (rows - 1, cols - 1))
    return $ foldl (\g (r, c) -> placeMine g r c) grid minePositions

placeMine :: Grid -> Int -> Int -> Grid
-- placeMine grid row col = take row grid ++ [take col (grid !! row) ++ [Mine] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid
placeMine grid row col = updateCell grid row col Mine

cellToString :: Cell -> String
cellToString FlagMine = printf "%-3s" ("🚩" :: String)
cellToString FlagRevealed = printf "%-3s" ("🚩" :: String)
cellToString Mine = printf "%-3s" ("💣" :: String)
cellToString Empty = printf "%-3s" ("🐥" :: String)
cellToString (Revealed 0) = printf "%-3s" ("🍀" :: String)
cellToString (Revealed n) = printf "%-4d" (n :: Int)

printGrid :: Grid -> IO()
printGrid grid = do
    let colNumbers = "   " ++ concatMap (\n -> printf "%-4d" (n :: Int)) [0..(length (head grid) - 1)]
        rowWithNumbers = zipWith (\num row -> printf "%-3d" (num :: Int) ++ concatMap cellToString row) [0..] grid
    putStrLn colNumbers
    mapM_ putStrLn rowWithNumbers

setLevel :: String -> [Int]
setLevel level = case level of
    "B" -> [8, 8, 10]
    "I" -> [16, 16, 40]
    "E" -> [30, 16, 99]

countMines :: Grid -> Int -> Int -> Int
countMines grid row col = length[() | (r, c) <- neighbours, isMine r c]
    where 
        neighbours = getNeighbours grid row col
        isMine r c = case preventErrors grid r c of
                       Just FlagMine -> True
                       Just Mine -> True
                       _         -> False

-- countCell :: Grid -> Cell -> Int
-- countCell grid cell = length [() | (r, c) <- gridCoords grid, isCell r c cell]
--     where
--         isCell r c cell = case preventErrors grid r c of
--                         Just cell -> True
--                         _         -> False 

gridCoords :: Grid -> [(Int, Int)]
gridCoords grid = do
    let
        rows = length grid
        cols = length (head grid)
    [ (r, c) | r <- [0..rows + 0], c <- [0..cols + 0]]

getNeighbours :: Grid -> Int -> Int -> [(Int, Int)]
getNeighbours grid row col = [ (r, c) | r <- [row - 1..row + 1], c <- [col - 1..col + 1], r >= 0, c >= 0, r < rows, c < cols, (r, c) /= (row, col) ] 
    where
        rows = length grid
        cols = length (head grid)

--revealCell :: Grid -> Int -> Int -> Grid
--revealCell grid row col = 
  --  case grid !!row !!col of
    --    Empty -> take row grid ++ [take col (grid !! row) ++ [Revealed (countMines grid row col)] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

flagCell :: Grid -> [(Int, Int)] -> Grid
flagCell grid [(row, col)] = 
    case grid !! row !! col of
        FlagRevealed -> updateCell grid row col Empty
        FlagMine -> updateCell grid row col Mine
        Empty -> updateCell grid row col FlagRevealed
        Mine -> updateCell grid row col FlagMine
        _    -> grid

revealCell :: Grid -> [(Int, Int)] -> Grid
revealCell grid [] = grid
revealCell grid ((row, col) : rest) =
    case grid !! row !! col of
        Empty -> let
                    newGrid = updateCell grid row col (Revealed (countMines grid row col ))
                    newRest = if countMines grid row col == 0 
                              then rest ++ (getNeighbours grid row col) 
                              else rest
                 in revealCell newGrid newRest
        _     -> revealCell grid rest

updateCell :: Grid -> Int -> Int -> Cell -> Grid
updateCell grid row col cell = 
    take row grid ++ [take col (grid !! row) ++ [cell] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

hideMines :: Grid -> Grid
hideMines grid = map (map hideCell) grid
        where
                hideCell Mine = Empty
                hideCell cell = cell

preventErrors :: Grid -> Int -> Int -> Maybe Cell
preventErrors grid r c
  | r < 0 || c < 0 || r >= length grid || c >= length (head grid) = Nothing
  | alreadyRevealed = Nothing
  | otherwise = Just (grid !! r !! c)
 where
         alreadyRevealed = case grid !! r !! c of
                            --  Flag       -> True
                             Revealed _ -> True
                             _          -> False
playGame :: Grid -> IO ()
playGame grid = do
    printGrid $ hideMines grid
    putStrLn "Enter row col mode (f: flag (optional)) (eg: 1 2 f or 1 2): "
    input <- getLine
    let (row, col, mode) = readCoords input
    case preventErrors grid row col of
        Just Mine -> if mode /= "f" then putStrLn "Boom! Game Over! You hit a mine." else do
            let newGrid = flagCell grid [(row, col)]
            playGame newGrid
        Just _ -> do
                --   putStrLn mode
                  let pos = [(row, col)]
                  let newGrid = if mode == "f" then (flagCell grid pos) else (revealCell grid pos)
                  playGame newGrid
        Nothing -> do
            putStrLn "Invalid Coordinates."
            playGame grid

readCoords :: String -> (Int, Int, String)
readCoords input = (read (words input !! 0), read (words input !! 1), if length (words input) == 3 then (words input !! 2) else "") 

main :: IO ()
main = do
    -- let rows = 5
    -- let cols = 5
    -- let numMines = 5
    putStrLn "Enter level (B: Beginner, I: Intermediate, E: Expert): "
    level <- getLine
    let [rows, cols, numMines] = setLevel level
    grid <- initializeGrid rows cols
    gridWithMines <- placeMines grid numMines
    let gridIndex = [ (r, c) | r <- [0..rows - 1], c <- [0..cols - 1] ] 
    let finalGrid = revealCell gridWithMines gridIndex
    --printGrid gridWithMines
    --putStrLn ""
    --let new = revealCell gridWithMines 6 1
    --printGrid new
    --putStrLn ""
    playGame gridWithMines
    printGrid finalGrid
    
