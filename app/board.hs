import System.Random (randomRIO)
import Control.Monad (replicateM)

type Grid = [[Cell]]
data Cell = Mine | Empty deriving (Show, Eq)
initializeGrid :: Int -> Int -> IO Grid
initializeGrid rows cols = return $ replicate rows (replicate cols Empty)

printGrid :: Grid -> IO ()
printGrid grid = mapM_ (putStrLn.concatMap showCell) grid
 where
    showCell Mine  = "* "
    showCell Empty = ". "

main :: IO ()
main = do
    let rows = 5
    let cols = 5
    grid <- initializeGrid rows cols
    printGrid grid
