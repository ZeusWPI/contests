import Control.Applicative ((<$>))
import Control.Monad (replicateM, replicateM_)
import Data.List (foldl')
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int)
type Level = Int
type Grid = Map Pos Level

neighbours :: Pos -> Set Pos
neighbours (r, c) = S.fromList [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]

floodLevel :: Grid -> (Int, Int) -> Level -> Int
floodLevel grid (height, width) level =
    S.size $
    S.difference drainable $
    flip flood' S.empty $ S.fromList $
        [(0, c)          | c <- [0 .. width - 1]]  ++
        [(r, width - 1)  | r <- [0 .. height - 1]] ++
        [(height - 1, c) | c <- [0 .. width - 1]]  ++
        [(r, 0)          | r <- [0 .. height - 1]]
  where
    flood' visit visited
        | S.null visit' = visited
        | otherwise     =
            flood' (visit' `S.union` neighbours next) visited'
      where
        next     = S.findMin visit'
        visited' = S.insert next visited
        visit'   = S.filter (not . blocked) $
            S.filter (`S.notMember` visited) visit

    drainable = S.filter (not . blocked) $ M.keysSet grid

    blocked pos = case M.lookup pos grid of
        Nothing -> True
        Just l  -> l > level

flood :: Grid -> (Int, Int) -> Int
flood grid (height, width) = sum
    [ floodLevel grid (height, width) l
    | l <- [0 .. maximum $ map snd $ M.toList grid]
    ]

parseGrid :: [String] -> Grid
parseGrid = M.map read .  foldl' rows M.empty . zip [0 ..] . map words
  where
    rows grid (r, row) = foldl' (cols r) grid $ zip [0 ..] row
    cols r grid (c, x) = M.insert (r, c) x grid

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [h, w] <- map read . words <$> getLine
        grid   <- parseGrid <$> replicateM h getLine
        print $ flood grid (h, w)
