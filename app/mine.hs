import System.Random (randomRIO)
import Control.Monad (replicateM)
import Text.Printf (printf)
type Grid = [[Cell]]
data Cell = Mine | Empty | Revealed Int deriving (Show, Eq)


initializeGrid :: Int -> Int -> IO Grid 
initializeGrid rows cols = return $ replicate rows (replicate cols Empty)

placeMines :: Grid -> Int -> IO Grid
placeMines grid numMines = do
    let rows = length grid
    let cols = length (head grid)
    minePositions <- replicateM numMines $ randomRIO ((0, 0), (rows - 1, cols - 1))
    return $ foldl (\g (r, c) -> placeMine g r c) grid minePositions

placeMine :: Grid -> Int -> Int -> Grid
placeMine grid row col = take row grid ++ [take col (grid !! row) ++ [Mine] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

cellToString :: Cell -> String
cellToString Mine = printf "%-3s" ("💣" :: String)
cellToString Empty = printf "%-3s" ("🐥" :: String)
cellToString (Revealed 0) = printf "%-3s" ("🍀" :: String)
cellToString (Revealed n) = printf "%-4d" (n :: Int)

cellToStringWithFlag :: [(Int, Int)] -> Int -> Int -> Cell -> String
cellToStringWithFlag flaggedCoords row col cell =
  if (row, col) `elem` flaggedCoords
  then printf "%-3s" ("🚩" :: String)
  else cellToString cell

printGrid :: Grid -> [(Int, Int)]-> IO()
printGrid grid flaggedCoords = do
    let colNumbers = "   " ++ concatMap (\n -> printf "%-4d" (n :: Int)) [0..(length (head grid) - 1)]
        rowWithNumbers = zipWith (\num row -> printf "%-3d" (num :: Int) ++ concatMap (\(col, cell) -> cellToStringWithFlag flaggedCoords num col cell) (zip [0..] row)) [0..] grid
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
                       Just Mine -> True
                       _         -> False

getNeighbours :: Grid -> Int -> Int -> [(Int, Int)]
getNeighbours grid row col = [ (r, c) | r <- [row - 1..row + 1], c <- [col - 1..col + 1], r >= 0, c >= 0, r < rows, c < cols, (r, c) /= (row, col) ] 
    where
        rows = length grid
        cols = length (head grid)

revealCell :: Grid -> [(Int, Int)] -> Grid
revealCell grid [] = grid
revealCell grid ((row, col) : rest) =
    case grid !! row !! col of
        Empty -> let
                    newGrid = updateCell grid row col (Revealed (countMines grid row col))
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
                             Revealed _ -> True
                             _          -> False

playGame :: Grid-> [(Int, Int)] -> IO ()
playGame grid flaggedCoords = do
    printGrid ( hideMines grid) flaggedCoords
    putStrLn "Enter row and col to reveal (eg. 1 2) or flag/unflag (f 1 2): "
    input <- getLine
    let wordsInput = words input
    if head wordsInput == "f"
    then do
        let (row, col) = readCoords (unwords (tail wordsInput))
        playGame grid flaggedCoords
    else do
        let (row, col) = readCoords input
        case preventErrors grid row col of
            Just Mine -> putStrLn "Boom! Game Over! You hit a mine." 
            Just _    -> do
                let newGrid = revealCell grid [(row, col)]
                playGame newGrid flaggedCoords
            Nothing   -> putStrLn "Invalid Coordinates." >> playGame grid flaggedCoords


readCoords :: String -> (Int, Int)
readCoords input = (read (words input !! 0), read (words input !! 1)) 

main :: IO ()
main = do
    putStrLn "Enter level (B: Beginner, I: Intermediate, E: Expert): "
    level <- getLine
    let [rows, cols, numMines] = setLevel level
    grid <- initializeGrid rows cols
    gridWithMines <- placeMines grid numMines
    let gridIndex = [ (r, c) | r <- [0..rows - 1], c <- [0..cols - 1] ] 
    let finalGrid = revealCell gridWithMines gridIndex
    playGame gridWithMines []
    printGrid finalGrid []
