import Data.List
import Data.Char
import Control.Monad

compress :: String -> String
compress = concatMap compress' . group
  where
    compress' [] = []
    compress' (x : []) = (x : [])
    compress' (y : x : []) = (y : x : [])
    compress' (z : y : x : []) = (z : y : x : [])
    compress' cs =
        '-' : idx (length $ take 26 cs) : head cs : compress' (drop 26 cs)

idx :: Int -> Char
idx i = chr (ord 'A' + i - 1)

decompress :: String -> String
decompress [] = []
decompress ('-' : n : c : ys) = replicate (count n) c ++ decompress ys
decompress (x : xs) = x : decompress xs 

count :: Char -> Int
count c = ord c - ord 'A' + 1

main :: IO ()
main = do
    n <- fmap read getLine
    replicateM_ n $ do
        line <- getLine
        case words line of
            ["???", x] -> out (decompress x) x
            [x, "???"] -> out x (compress x)
            _          -> return ()
  where
    out x y = putStrLn $ x ++ " " ++ y
