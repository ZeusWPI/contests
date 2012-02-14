import Control.Monad (replicateM_)

cubes :: Int -> Int
cubes n = sum [x * x * x | x <- [1 .. n]]

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        n <- readLn
        print $ cubes n
