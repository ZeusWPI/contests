import Control.Monad (replicateM_)
import Data.Char (ord, chr, isDigit, isPunctuation)

ord' :: Char -> Int
ord' ' ' = 26
ord' c   = ord c - ord 'A'

chr' :: Int -> Char
chr' 26 = ' '
chr' n  = chr $ n + ord 'A'

decode :: Int -> Char -> Char
decode n c
    | isPunctuation c = c 
    | isDigit c       = c
    | otherwise       = chr' $ mod (ord' c - n) 27

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        line <- getLine
        let (n, str) = break (== ' ') line
        putStrLn $ map (decode $ read n) $ tail str
