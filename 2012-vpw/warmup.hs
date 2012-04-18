import Control.Monad
import Data.List

main :: IO ()
main = do
    n   <- readLn
    arr <- replicateM n readLn
    let (min' , max')   = foldl1' f $ zip arr arr :: (Int, Int)
        f (a, b) (x, y) = a `seq` b `seq` (min a x, max b y)

    putStrLn $ show min' ++ " " ++ show max'

