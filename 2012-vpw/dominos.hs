{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative ((<$>))
import Control.Exception (IOException, catch)
import Control.Monad (replicateM, replicateM_)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Prelude hiding (catch)
import qualified Data.Map as M
import qualified Data.Set as S

type Domino = (Int, Int)
type Pos = (Int, Int)
type Grid = Map Pos Int

-- | Grab dominos with the given numbers
grab :: Int -> Int -> Set Domino -> [Domino]
grab t1 t2 left = filter (`S.member` left) $
    if t1 == t2 then [(t1, t2)] else [(t1, t2), (t2, t1)]

solve :: (Int, Int) -> Grid -> Set Domino -> Pos -> Int
solve (width, height) grid left pos1@(x1, y1)
    | x1 >= width             = solve (width, height) grid left (0, y1 + 1)
    | y1 >= height            = 1
    | pos1 `M.notMember` grid = solve (width, height) grid left (x1 + 1, y1)
    | otherwise               = solve' (x1 + 1, y1) + solve' (x1, y1 + 1)
  where
    solve' pos2 = sum
        [ n
        | t1     <- maybeToList $ M.lookup pos1 grid
        , t2     <- maybeToList $ M.lookup pos2 grid
        , domino <- grab t1 t2 left
        , let left' = S.delete domino left
        , let grid' = M.delete pos1 $ M.delete pos2 grid
        , let n     = solve (width, height) grid' left' (x1 + 1, y1)
        ] 

parseGrid :: [String] -> Grid
parseGrid = M.map read . foldl' rows M.empty . zip [0 ..] . map words
  where
    rows grid (r, row) = foldl' (cols r) grid $ zip [0 ..] row
    cols r grid (c, x) = M.insert (c, r) x grid

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        grid <- parseGrid <$> replicateM 7 getLine
        _    <- catch getLine (\(_ :: IOException) -> return "")
        let left = S.fromList [(x, y) | x <- [0 .. 6], y <- [x .. 6]]
            n    = solve (8, 7) grid left (0, 0)
        print n
