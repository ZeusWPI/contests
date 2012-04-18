import Control.Monad
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as M

import Data.List

buildMap :: [String] -> Map String [String]
buildMap words = M.fromList [ (w, l) | w' <- words, let w = init w', let l = findMatch w words ]
  where
    findMatch word words = filter (word `isPrefixOf`) words

listToMaybe [] = Nothing
listToMaybe a = Just a

removeMap :: String -> Map String [String] -> Map String [String]
removeMap str map =
    let index = init str
    in M.update (\arr -> listToMaybe $ delete str arr) index map

findWords:: Map String [String] -> [String] -> [String]
findWords m [] = []
findWords m (w:t) =
    (findWord (removeMap w m) w (tail w)) ++ findWords m t

findWord :: Map String [String] -> String -> String -> [String]
findWord m word substr
    | M.null m = [word]
    | otherwise =
        case M.lookup substr m of
            Nothing -> []
            Just list -> concatMap (\opt -> let m' = removeMap opt m
                                 in findWord m' (word ++ [last opt]) (tail opt)) list

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
       w <- words <$> getLine
       let m = buildMap w
       putStrLn $ head $ findWords m w
