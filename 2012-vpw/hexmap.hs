import Control.Applicative ((<$>))
import Control.Monad (replicateM, replicateM_)

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [(w, h, f), (_, _, m), (_, _, b)] <- replicateM 3 getGrid
        putStrLn $ unwords $ map show [w, h]
        mapM_ putStrLn $ cut w $ zipWith3 hexmap f m b
  where
    getGrid = do
        [w, h] <- map read . words <$> getLine
        chars  <- concat <$> replicateM h getLine
        return (w, h, chars)

    cut _ [] = []
    cut n xs = let (h, t) = splitAt n xs in h : cut n t

    hexmap _ '0' b = b
    hexmap f 'F' _ = f
    hexmap _ m   _ = error $ "Unknown mask char: " ++ show m
