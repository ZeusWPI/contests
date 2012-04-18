import Control.Monad
import Control.Applicative
import Data.List

data Tree = Tree String [Tree]
  deriving (Show, Eq)

updateTree :: Tree -> Tree -> Tree
updateTree (Tree a children) newTree@(Tree b children')
  | a == b    = newTree
  | otherwise = Tree a $ map (\c -> updateTree c newTree) children

parseNode :: [String] -> Tree
parseNode words = Tree (head words) (children $ drop 2 words)
  where children :: [String] -> [Tree]
        children []    = []
        children (h:t) = (Tree h []) : children t

isBottom (Tree _ []) = True
isBottom _           = False

removeNode :: Tree -> [Tree]
removeNode (Tree a children) = concat [removeNode i | i <- filter (not . isBottom) children] ++
                               [Tree a (delete i children) | i <- filter isBottom children]

solve :: Tree -> Int -> Int -> Int
solve (Tree root []) acc  m      = m
solve (Tree root children) acc m
    | acc >= m                   = m
    | otherwise = go children m
        where go [] m = acc
              go (c:cs) m = let children' = delete c cs
                                min = go' (removeNode c) children' m
                            in go cs min

              go' [] children' min = solve (Tree root children') (acc + 1) min
              go' ops children' min = go'' ops children' min

              go'' [] children' min = min
              go'' (op:ops) children' min = let min' = solve (Tree root (op:op:children')) (acc + 1) min
                                            in go'' ops children' min'
main = do
    cases <- readLn
    replicateM_ cases $ do
        n  <- readLn
        root <- parseNode . words <$> getLine
        nodes <- replicateM (n-1) (parseNode . words <$> getLine)
        let parsed = foldl' updateTree root nodes
        print parsed
        print $ solve parsed 0 2000
