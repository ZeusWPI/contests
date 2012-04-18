import Control.Applicative ((<$>))
import Control.Monad (replicateM, replicateM_)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

data Tile = Empty | Char Char | Slash | Backslash
    deriving (Eq, Show)

fromGrid :: Map (Int, Int) Char -> Map (Int, Int) Tile
fromGrid = M.map parseTile
  where
    parseTile ' '  = Empty
    parseTile '/'  = Slash
    parseTile '\\' = Backslash
    parseTile x    = Char x

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

trace :: Map (Int, Int) Tile
      -> Tile
      -> (Int, Int)
      -> Dir
      -> Bool
trace grid goal pos dir = case tile of
    Char x -> Char x == goal
    _      -> trace grid goal pos' dir'
  where
    pos' = pos .+ dir
    tile = fromMaybe Empty $ M.lookup pos' grid
    dir' = case tile of
        Slash     -> slash dir
        Backslash -> backslash dir
        _         -> dir

starts :: Int
       -> Int
       -> Map (Int, Int) Tile
       -> [(Tile, (Int, Int), Dir)]
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
        grid   <- fromGrid . parseGrid <$> replicateM (h + 2) getLine
        let ok = all (\(c, p, d) -> trace grid c p d) $ starts h w grid
        putStrLn $ if ok then "correct" else "verkeerd"
