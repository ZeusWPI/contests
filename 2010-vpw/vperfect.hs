import Control.Applicative ((<$>))
import Control.Monad (replicateM_)

divisors :: Int -> [Int]
divisors n = fmap sum' [ (x,n `div` x) | x <- [1..(floor $ sqrt $ fromIntegral n)], n `mod` x == 0]
  where
    sum' (x,y)
        | x == y    = x
        | otherwise = x + y


vperfect :: Int -> Int -> [(Int, Int)]
vperfect x y = [ (n, sd `div` n) | n <- [x..y] , let sd = sum $ divisors n, sd `mod` n == 0 ]

main = do
    n <- read <$> getLine
    replicateM_ n $ do
        [x,y] <- (fmap read) . words <$> getLine
        case vperfect x y of
            []        -> putStrLn "GEEN"
            ((x,y):_) -> putStrLn $ show x ++ " " ++ show y
