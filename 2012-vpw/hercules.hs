import Control.Applicative ((<$>))
import Control.Monad (replicateM, replicateM_)
import Data.Map (Map)
import qualified Data.Map as M

type Forest = Map String Node
data Branch = Empty | Branch String deriving (Show)
data Node   = Node [Branch]         deriving (Show)

parseNode :: String -> (String, Node)
parseNode line = case words line of
    (name : _ : bs) -> (name, Node (map parseBranch bs))
    _               -> error $ "parseNode: Can't parse " ++ line
  where
    parseBranch "*" = Empty
    parseBranch x   = Branch x

parseForest :: [String] -> Forest
parseForest = foldl (\f (n, node) -> M.insert n node f) M.empty . map parseNode

size :: Forest -> Branch -> Int
size _      Empty         = 1
size forest (Branch name) = case M.lookup name forest of
    Just (Node branches) -> 1 + sum (map (size forest ) branches)
    _                    -> error $ "size: Unknown branch: " ++ name

hercules :: Forest -> Int
hercules forest = case M.lookup "stam" forest of
    Nothing              -> error "hercules: no root"
    Just (Node branches) -> sum $ map (go . size forest) branches
  where
    go 1 = 1
    go n = go (n - 1) * 2 + 1

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        n      <- readLn
        forest <- parseForest <$> replicateM n getLine
        print $ hercules forest
