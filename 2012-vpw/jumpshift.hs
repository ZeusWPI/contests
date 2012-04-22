import Control.Applicative ((<$>))
import Control.Monad (replicateM_)

data T = A | B | E deriving (Show)

makeTiles :: Char -> String -> [T]
makeTiles a = map makeTile . (if a == 'W' then id else reverse)
  where
    makeTile 'L' = E
    makeTile x   = if x == a then A else B

moves :: (Int, Int) -> [T] -> (Int, Int)
moves (j, s) []               = (j, s)
moves (j, s) (A : E : xs)     = moves (j, s + 1) xs
moves (j, s) (A : B : E : xs) = moves (j + 1, s) xs
moves (j, s) (_ : xs)         = moves (j, s) xs

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [[a], cs] <- words <$> getLine
        let (j, s) = moves (0, 0) $ makeTiles a cs
        putStrLn $ unwords $ map show [j, s]
