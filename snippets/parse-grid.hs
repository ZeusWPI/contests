-- | Useful for parsing data of the form
--
-- > XOXOXOXOX
-- > OXOXXXXOX
-- > XOXOXXXOO
--
-- Into a map which is indexed on (row, column), both zero-based.
--
-- And obviously also a function to do the reverse mapping.
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

parseGrid :: [String]             -- ^ Lines
          -> Map (Int, Int) Char  -- ^ Resulting grid
parseGrid = foldl' rows M.empty . zip [0 ..]
  where
    rows grid (r, row) = foldl' (cols r) grid $ zip [0 ..] row
    cols r grid (c, x) = M.insert (r, c) x grid

unparseGrid :: Char                 -- ^ Default character if none is present
            -> Int                  -- ^ Height
            -> Int                  -- ^ Width
            -> Map (Int, Int) Char  -- ^ Grid
            -> [String]             -- ^ String representation
unparseGrid def height width grid =
    [ [fromMaybe def (M.lookup (r, c) grid) | c <- [0 .. width - 1]]
    | r <- [0 .. height - 1]
    ]

-- | Usage example
main :: IO ()
main = do
    -- Read height, width (typically given)
    [h, w] <- map read . words <$> getLine
    grid   <- parseGrid <$> replicateM h getLine
    let grid' = M.delete (h `div` 2, w `div` 2) grid
    mapM_ putStrLn $ unparseGrid '?' h w grid'
