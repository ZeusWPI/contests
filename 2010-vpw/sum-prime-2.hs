import Control.Monad (mplus, replicateM_)
import Data.List (intercalate)

primes :: Int -> [Int]
primes n = go [2 .. n]
  where
    go []       = []
    go (x : xs) = x : go (filter (not . (== 0) . (`mod` x)) xs)

solve :: Int -> Maybe (Int, [Int])
solve goal = go 0 0 1000 [] (reverse $ primes 100)
  where
    go _    _   _    _    []                  = Nothing
    go sum' num min' list (p : ps)
        | sum' > goal                         = Nothing
        | (goal - sum') `div` p + num >= min' = Nothing
        | sum' == goal                        = Just (num, list)
        | otherwise                           =
            let b1    = go (sum' + p) (num + 1) min' (p : list) (p : ps)
                min'' = maybe min' fst b1
                b2    = go sum' num min'' list ps
            in b2 `mplus` b1

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        x <- readLn
        let Just (_, ps) = solve x
        putStrLn $ intercalate "+" $ map show $ reverse ps
