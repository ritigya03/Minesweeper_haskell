import System.Random (randomRIO)
import Control.Monad (replicateM)
  4 type Grid = [[Cell]]
  5 data Cell = Mine | Empty deriving (Show, Eq)
  6
  7
  8 initializeGrid :: Int -> Int -> IO Grid
  9 initializeGrid rows cols = return $ replicate rows (replicate cols Empty)
 10
 11
 12 placeMines :: Grid -> Int -> IO Grid
 13 placeMines grid numMines = do
 14     let rows = length grid
 15     let cols = length (head grid)
 16     minePositions <- replicateM numMines $ randomRIO ((0, 0), (rows - 1, cols - 1))
 17     return $ foldl (\g (r, c) -> placeMine g r c) grid minePositions
 18
 19
 20 placeMine :: Grid -> Int -> Int -> Grid
 21 placeMine grid row col = take row grid ++ [take col (grid !! row) ++ [Mine] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid
 22
 23
 24 printGrid :: Grid -> IO ()
 25 printGrid grid = mapM_ (putStrLn . concatMap showCell) grid
 26   where
 27     showCell Mine  = "* "
 28     showCell Empty = ". "
 29
 30 main :: IO ()
 31 main = do
 32     let rows = 5
 33     let cols = 5
 34     let numMines = 5
 35     grid <- initializeGrid rows cols
 36     gridWithMines <- placeMines grid numMines
 37     printGrid gridWithMines