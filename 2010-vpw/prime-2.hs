import Control.Applicative
import Control.Monad.State
import Data.Bits
import Data.Int

sieve :: Integral a => a -> [a]
sieve s = sieve' [2..s]
  where
    sieve' []       = []
    sieve' (y : ys) = y : (sieve' . filter (\l -> l `mod` y /= 0) $ ys)

primes n m = dropWhile (< n) $ takeWhile (<= m) $ sieve m

nearestPower :: Int32 -> Int32
nearestPower n = nearestPower' (n - 1) 1
  where
    nearestPower' n 16 = (n .|. (n `shiftR` 16)) + 1
    nearestPower' n m = nearestPower' (n .|. (n `shiftR` m)) (m * 2)


distanceToPower n
    | higher - n < n - lower = (n, higher - n)
    | otherwise = (n, n - lower)
  where
    higher = nearestPower n
    lower = higher `div` 2

findPrime :: Int32 -> Int32 -> Maybe (Int32, Int32)
findPrime n m
    | null primes' = Nothing
    | otherwise    = return $ foldr1 lowest $ map distanceToPower $ primes'
  where
    lowest (l,ld) (n, d)
        | d < ld = (n, d)
        | otherwise = (l,ld)

    primes' = primes n m


main :: IO ()
main = do
    n <- read <$> getLine
    replicateM_ n $ do
        [n,m] <- map read <$> words <$> getLine
        let p = findPrime n m
        case p of
            Just (prime, d) -> print $ prime
            _               -> putStrLn $ "geen priemgetal gevonden"

