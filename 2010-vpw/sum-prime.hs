import Control.Applicative
import Control.Monad.State

import Data.List (intercalate)

sieve :: Integral a => a -> [a]
sieve s = sieve' [2..s]
  where
    sieve' []       = []
    sieve' (y : ys) = y : (sieve' . filter (\l -> l `mod` y /= 0) $ ys)

erasto :: [Int]
erasto = sieve 100

solve :: Int -> (Int, [Int])
solve n = solve' n primes n (0, [])
  where
    primes = takeWhile (<= n) erasto

solve' :: Int -> [Int] -> Int -> (Int,[Int]) -> (Int, [Int])
solve' 0   _      _      (len, s) = (len, s)
solve' _   []     _      (len, s) = (len, s)
solve' num primes minlen (len, s) =
    foldr f (minlen, s) primes'
  where
    -- only take primes lower than num
    primes' = takeWhile (<= num) primes

    minLenReq p = num `div` p

    f pr (ml,res)
        | ml <= len + minLenReq pr = (ml, res)
        | l' < ml                  = (l', res')
        | otherwise                = (ml, res)
      where
        (l', res') = solve' (num - pr) primes' ml ((len + 1), (pr : s))

main :: IO ()
main = do
    n <- read <$> getLine
    replicateM_ n $ do
        num <- read <$> getLine
        let (_, ar) = solve num
        putStrLn $ intercalate "+" $ map show $ reverse ar
