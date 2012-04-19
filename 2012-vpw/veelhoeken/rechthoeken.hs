module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace

type Point = (Int, Int)
type Line = (Point, Point)

type Rect = [Point]

rect :: Point -> Point -> Point -> Point -> Rect
rect p1 p2 p3 p4 = sort [p1, p2, p3, p4]

horizontal :: Line -> Bool
horizontal ((_, y1), (_, y2)) = y1 == y2

inter :: Line -> Line -> Maybe Point
inter ((x1, y), (x2, _)) ((x, y1), (_, y2))
    | x1 <= x && x <= x2 && y1 <= y && y <= y2 = Just (x, y)
    | otherwise                                = Nothing

pairs :: [Line] -> [Line] -> [(Line, Line)]
pairs horis verts =
    [ (h, v)
    | h <- horis
    , v <- verts
    , isJust $ inter h v
    ]

rects :: [Line] -> Set Rect
rects lines' = S.fromList
    -- [ traceShow (rect p1 p2 p3 p4, h1, v1, h2, v2) $ rect p1 p2 p3 p4
    [ rect p1 p2 p3 p4
    | h1 <- horis'
    , h2 <- horis'
    , h1 /= h2
    , v1 <- verts'
    , v2 <- verts'
    , v1 /= v2
    , p1 <- maybeToList $ inter h1 v1
    , p2 <- maybeToList $ inter h1 v2
    , p3 <- maybeToList $ inter h2 v1
    , p4 <- maybeToList $ inter h2 v2
    , length (nub [p1, p2, p3, p4]) == 4  -- Unique
    ]
  where
    (horis, verts) = partition horizontal lines'
    horis' = melt extendsHori extendHori horis
    verts' = melt extendsVert extendVert verts

melt :: (Line -> Line -> Bool)
     -> (Line -> Line -> Line)
     -> [Line]
     -> [Line]
melt extends extend ls = case ls of
    []         -> []
    (l1 : ls') -> case find (extends l1) ls' of
        Just l2 -> melt extends extend $ extend l1 l2 : delete l2 ls'
        Nothing -> l1 : melt extends extend ls'

extendsHori :: Line -> Line -> Bool
extendsHori ((x1, y), (x2, _)) ((x1', y'), (x2', _)) =
    y == y' &&
    ((x1 <= x1' && x1' <= x2) || (x1 <= x2' && x2' <= x2))

extendHori :: Line -> Line -> Line
extendHori ((x1, y), (x2, _)) ((x1', _), (x2', _)) =
    ((min x1 x1', y), (max x2 x2', y))

extendHoris :: [Line] -> [Line]
extendHoris ls =
    [ ((min x1 x1', y), (max x2 x2', y))
    | l1@((x1, y), (x2, _))    <- ls
    , l2@((x1', y'), (x2', _)) <- ls
    , l1 /= l2
    , y == y'
    , (x1 <= x1' && x1' <= x2) || (x1 <= x2' && x2' <= x2)
    ]

extendsVert :: Line -> Line -> Bool
extendsVert ((x, y1), (_, y2)) ((x', y1'), (_, y2')) =
    x == x' &&
    ((y1 <= y1' && y1' <= y2) || (y1 <= y2' && y2' <= y2))

extendVert :: Line -> Line -> Line
extendVert ((x, y1), (_, y2)) ((_, y1'), (_, y2')) =
    ((x, min y1 y1'), (x, max y2 y2'))

extendVerts :: [Line] -> [Line]
extendVerts ls =
    [ ((x, min y1 y1'), (x, max y2 y2'))
    | l1@((x, y1), (_, y2))    <- ls
    , l2@((x', y1'), (_, y2')) <- ls
    , l1 /= l2
    , x == x'
    , (y1 <= y1' && y1' <= y2) || (y1 <= y2' && y2' <= y2)
    ]

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        n  <- readLn
        ls <- replicateM n line
        print $ S.size $ rects ls
  where
    line = do
        [x1, y1, x2, y2] <- map read . words <$> getLine
        return ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))
