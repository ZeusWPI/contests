import Control.Applicative ((<$>))
import Control.Monad (replicateM, replicateM_)
import Data.Char (isAlpha)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data Dir = U | R | D | L
    deriving (Show)

(.+) :: (Int, Int) -> Dir -> (Int, Int)
(r, c) .+ U = (r - 1, c)
(r, c) .+ R = (r, c + 1)
(r, c) .+ D = (r + 1, c)
(r, c) .+ L = (r, c - 1)

slash :: Dir -> Dir
slash U = R
slash R = U
slash D = L
slash L = D

backslash :: Dir -> Dir
backslash U = L
backslash R = D
backslash D = R
backslash L = U

trace :: Map (Int, Int) Char
      -> Char
      -> (Int, Int)
      -> Dir
      -> Bool
trace grid goal pos dir
    | isAlpha tile = tile == goal
    | otherwise    = trace grid goal pos' dir'
  where
    pos' = pos .+ dir
    tile = fromMaybe '?' $ M.lookup pos' grid
    dir' = case tile of
        '/'  -> slash dir
        '\\' -> backslash dir
        _    -> dir

starts :: Int
       -> Int
       -> Map (Int, Int) Char
       -> [(Char, (Int, Int), Dir)]
starts height width grid =
    [start 0            c           D | c <- [1 .. width]]  ++  -- Top
    [start r            (width + 1) L | r <- [1 .. height]] ++  -- Right
    [start (height + 1) c           U | c <- [1 .. width]]  ++  -- Bottom
    [start r            0           R | r <- [1 .. height]]     -- Left
  where
    start r c d = (grid M.! (r, c), (r, c), d)

parseGrid :: [String]             -- ^ Lines
          -> Map (Int, Int) Char  -- ^ Resulting grid
parseGrid = foldl' rows M.empty . zip [0 ..]
  where
    rows grid (r, row) = foldl' (cols r) grid $ zip [0 ..] row
    cols r grid (c, x) = M.insert (r, c) x grid

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [h, w] <- map read . words <$> getLine
        grid   <- parseGrid <$> replicateM (h + 2) getLine
        let ok = all (\(c, p, d) -> trace grid c p d) $ starts h w grid
        putStrLn $ if ok then "correct" else "verkeerd"
