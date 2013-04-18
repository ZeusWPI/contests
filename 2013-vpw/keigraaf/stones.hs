import Data.Set (Set)
import qualified Data.Set as S
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad

type Node = Int

type Graph = Map Node (Set Node)

type Edge = (Int, Int)

fromNodes :: [Node] -> Graph
fromNodes nodes = M.fromList [(node, S.empty) | node <- nodes]

addEdge :: Edge -> Graph -> Graph
addEdge (x, y) =
    M.insertWith' S.union x (S.singleton y) .
    M.insertWith' S.union y (S.singleton x)

neighbours :: Node -> Graph -> [Node]
neighbours node = fromMaybe [] . fmap S.toList . M.lookup node

type Config = Map Node Int

stones :: Node -> Config -> Int
stones node = fromMaybe 0 . M.lookup node

addStones :: Node -> Int -> Config -> Config
addStones node stones = M.insertWith' (+) node stones

step :: Graph -> Config -> Config
step graph config = foldl' step M.empty (M.keys config)
  where
    step c node =
        let nbs = neighbours node graph
            (give, keep) = divMod (stones node config) (length nbs)
        in foldl' (\cn nb -> addStones nb give cn) (addStones node keep c) nbs

type History = Map Config Int

play :: History -> Graph -> Config -> Int -> Int
play history graph config current =
    let config'  = step graph config
        history' = M.insert config' current history
    in case M.lookup config' history of
        Just n -> current - n
        Nothing -> play history' graph config' (current + 1)

testCase :: IO ()
testCase = do
    numNodes <- readLn
    config <- fmap M.fromList $ forM [1 .. numNodes] $ \i -> do
        stones <- readLn
        return (i, stones)
    numEdges <- readLn
    edges <- replicateM numEdges $ do
        line <- getLine
        let [x, y] = words line
        return (read x, read y)
    let graph = foldl' (\g e -> addEdge e g) (fromNodes $ M.keys config) edges
        period = play M.empty graph config 1
    -- putStrLn $ "graph: " ++ show graph
    -- putStrLn $ "config: " ++ show config
    print period

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases testCase
