-- solution to http://www.vlaamseprogrammeerwedstrijd.be/2011/opgaves/cat3-2011/gesommeerde_priemgetallen.pdf
import Debug.Trace (traceShow)
import Control.Monad
import Control.Applicative
import Data.List

divisors :: Int -> Int
divisors n = length $ [ x | x <- [2..(floor $ sqrt $ fromIntegral n)] , n `mod` x == 0]

sieve :: Integral a => a -> [a]
sieve s = sieve' [2..s]
  where
    sieve' []       = []
    sieve' (y : ys) = y : (sieve' . filter (\l -> l `mod` y /= 0) $ ys)

isPrime 0 = False
isPrime 1 = False
isPrime n = divisors n == 0

primesBetween m n = dropWhile (< m) $ sieve n


solve m n = snd $ solve' n (0,m) (primesBetween m n) (0, 0)

solve' n res [] _ = res
solve' n (max,r) primes (l, res) =
    foldl find (max,r) (tails primes)
  where
    find :: (Int, Int) -> [Int] -> (Int,Int)
    find (max,r) pr = let (_,len,_) = foldl sumPrime (0,0,0) pr
                      in if len > max then (len,sum $ take len pr) else (max,r)
      where
        sumPrime (curlen, maxlen, cur) p
            | (cur + p) > n   = (maxlen, maxlen, cur)
            | isPrime (cur+p) = (curlen+1, curlen+1, cur+p)
            | otherwise       = (curlen+1 ,maxlen, cur+p)

main = do
    cases <- readLn
    replicateM_ cases $ do
        [m,n] <- map read . words <$> getLine
        print $ solve m n

