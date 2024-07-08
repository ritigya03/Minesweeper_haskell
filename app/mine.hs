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
        isMine r c = case preventErrors grid r c of
                       Just Mine -> True
                       _         -> False

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
                          ++ drop (col + 1) (grid !! row)]++ drop (row + 1) grid

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

playGame :: Grid -> IO()
playGame grid = do
        printGrid grid
        putStrLn "Enter row and col to reveal (eg. 1 2): "
        input <- getLine
        let (row, col) = readCoords input
        if preventErrors grid row col
           then do
                   let newGrid = revealCell grid row col
                   case preventErrors grid row col of
                     Just Mine -> putStrLn "Boom Game Over! You hit a mine." >> printGrid newGrid
                     _         -> playGame newGrid
           else putStrLn "Invalid Coordinates." >> playGame grid

readCoords :: String -> (Int, Int)
readCoords input = (read (words input !! 0), read (words input !! 1)) 

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
    printGrid $ hideMines gridWithMines
    putStrLn ""
    let new = revealCell gridWithMines 6 1
    printGrid new
    playGame gridWithMines
