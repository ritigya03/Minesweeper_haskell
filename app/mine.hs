import System.Random (randomRIO)
import Control.Monad (replicateM)
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

printGrid :: Grid -> IO()
printGrid grid = mapM_ (putStrLn . concatMap showCell) grid
    where
        showCell Mine = "💣 "
        showCell (Revealed 0) = "🍀 "
        showCell (Revealed n) = show n ++ " "
        showCell Empty = "🐥 "

setLevel :: String -> [Int]
setLevel level = case level of
    "B" -> [8, 8, 10]
    "I" -> [16, 16, 40]
    "E" -> [30, 16, 99]

countMines :: Grid -> Int -> Int -> Int
countMines grid row col = length[() | (r, c) <- neighbours, isMine r c]
    where 
        -- rows = length grid
        -- cols = length (head grid)
        -- neighbours = [(r, c) | r <- [row - 1..row + 1], c <- [col - 1..col + 1], r >= 0, c >= 0, r < rows, c < cols, (r, c) /= (row, col)]
        neighbours = getNeighbours grid row col
        isMine r c = grid !! r !! c == Mine

getNeighbours :: Grid -> Int -> Int -> [(Int, Int)]
getNeighbours grid row col = [ (r, c) | r <- [row - 1..row + 1], c <- [col - 1..col + 1], r >= 0, c >= 0, r < rows, c < cols, (r, c) /= (row, col) ] 
    where
        rows = length grid
        cols = length (head grid)

revealCell :: Grid -> Int -> Int -> Grid
revealCell grid row col = 
    case grid !!row !!col of
    Empty
            -> take row grid
                 ++
                   [take col (grid !! row)
                      ++
                        [Revealed (countMines grid row col)]
                          ++ drop (col + 1) (grid !! row)]
                     ++ drop (row + 1) grid
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
    printGrid gridWithMines
    putStrLn ""
    let new = revealCell gridWithMines 6 1
    printGrid new