--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (replicateM_)
import           Data.List           (foldl')
import           Data.List           (sortBy)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Ord            (comparing)
import qualified Data.Set            as S


--------------------------------------------------------------------------------
data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
manhattan :: Pos -> Pos -> Int
manhattan (Pos x1 y1) (Pos x2 y2) = abs (x1 - x2) + abs (y1 - y2)


--------------------------------------------------------------------------------
neighbours :: Pos -> [Pos]
neighbours (Pos x y) =
    [Pos (x - 1) y, Pos x (y - 1), Pos (x + 1) y, Pos x (y + 1)]


--------------------------------------------------------------------------------
type Puzzle = Map Pos Char


--------------------------------------------------------------------------------
emptyPos :: Puzzle -> Pos
emptyPos cw = case [pos | (pos, ' ') <- M.toList cw] of
    []      -> error "emptyPos: No empty pos"
    (x : _) -> x


--------------------------------------------------------------------------------
parsePuzzle :: (Int, Int) -> String -> Puzzle
parsePuzzle (_, width) str = M.fromList
    [ (Pos coln rown, c)
    | (row, rown) <- zip grid [1 ..]
    , (c, coln)   <- zip row [1 ..]
    ]
  where
    grid = every width str


--------------------------------------------------------------------------------
every :: Int -> [a] -> [[a]]
every n xs = case splitAt n xs of
    (ys, []) -> [ys]
    (ys, zs) -> ys : every n zs


--------------------------------------------------------------------------------
swap :: Pos -> Pos -> Puzzle -> Puzzle
swap p1 p2 cw =
    let x = cw M.! p1
        y = cw M.! p2
    in M.insert p1 y (M.insert p2 x cw)


--------------------------------------------------------------------------------
search :: Puzzle -> Puzzle -> Int -> Int
search goal initial maxSteps = go S.empty initial (emptyPos initial) 0 maxSteps
  where
    goalEmptyPos = emptyPos goal

    go visited current empty steps best
        | current == goal                                = min steps best
        | current `S.member` visited                     = best
        | steps + (manhattan empty goalEmptyPos) >= best = best
        | otherwise                                      =
            foldl' tryMove best moves
      where
        -- Sort the moves by manhattan distance and try them in that order.
        -- The filter ensures we only try moves within the grid.
        moves              = sortBy (comparing $ manhattan goalEmptyPos) $
            filter (`M.member` current) $ neighbours empty
        tryMove best' move =
            let best'' = go (S.insert current visited)
                            (swap empty move current)
                            move
                            (steps + 1)
                            best'
            in min best' best''


--------------------------------------------------------------------------------
testCase :: IO ()
testCase = do
    [height, width] <- map read . words <$> getLine
    line            <- getLine
    line'           <- getLine
    let puzzle = parsePuzzle (height, width) line
        goal   = parsePuzzle (height, width) line'
    print $ search puzzle goal 14


--------------------------------------------------------------------------------
main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases testCase
