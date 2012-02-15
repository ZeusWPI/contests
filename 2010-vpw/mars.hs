import Control.Applicative ((<$>))
import Control.Monad (replicateM, replicateM_)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.List (delete, minimumBy)
import Data.Ord (comparing)
import qualified Data.Map as M

import Debug.Trace (traceShow)

type Station = (Int, Int)

data Transport
    = Marsrover
    | Monorail
    deriving (Eq, Show)

-- | In m/s
speed :: Transport -> Double
speed Marsrover = 20  / 3.6
speed Monorail  = 200 / 3.6

type Graph = Map (Station, Station) Transport

getTransport :: Station -> Station -> Graph -> Transport
getTransport s1 s2 = fromMaybe Marsrover . M.lookup (s1, s2)

putTransport :: Station -> Station -> Transport -> Graph -> Graph
putTransport s1 s2 Marsrover = M.delete (s1, s2)
putTransport s1 s2 Monorail  = M.insert (s1, s2) Monorail

-- | In m
distance :: Station -> Station -> Double
distance (x1, y1) (x2, y2) = 1000 * sqrt (dx * dx + dy * dy)
  where
    dx = fromIntegral $ x2 - x1
    dy = fromIntegral $ y2 - y1

-- | In s
time :: Station -> Station -> Graph -> Double
time s1 s2 graph = distance s1 s2 / speed (getTransport s1 s2 graph)

sumTime :: Station -> [Station] -> Graph -> Double
sumTime s0 stations graph = sum $
    map snd $ M.toList $ dijkstra graph s0 stations M.empty

dijkstra :: Graph
         -> Station
         -> [Station]
         -> Map Station Double
         -> Map Station Double
dijkstra _      _ []       known = known
dijkstra graph s0 stations known =
    dijkstra graph next (delete next stations) known'
  where
    -- Distances when starting from the current station
    toHere = fromMaybe 0 $ M.lookup s0 known
    here   = M.fromList [(s, toHere + time s0 s graph) | s <- stations]
    known' = M.unionWith min known here
    next   = fst $ minimumBy (comparing snd) [(s, known' M.! s) | s <- stations]

optimal :: Station -> [Station] -> Graph -> Double -> Double
optimal s0 stations graph available
    | null possible = sumTime s0 stations graph
    | otherwise     = minimum
        [ optimal s0 stations graph' available'
        | (s1, s2) <- possible
        , let graph' = putTransport s1 s2 Monorail graph
        , let available' = available - distance s1 s2
        ]
  where
    possible =
        [ (s1, s2)
        | s1 <- s0 : stations
        , s2 <- stations
        , s1 < s2
        , getTransport s1 s2 graph /= Monorail
        , distance s1 s2 <= available
        ]

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [n, rail]       <- map read . words <$> getLine
        (s0 : stations) <- replicateM n $ do
            [x, y] <- map read . words <$> getLine
            return (x :: Int, y)

        let seconds = optimal s0 stations M.empty (fromIntegral rail * 1000)
            minutes = ceiling $ seconds / 60

        print (minutes :: Int)
