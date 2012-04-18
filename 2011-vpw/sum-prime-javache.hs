import Control.Applicative
import Control.Monad

import Data.Maybe
import Data.List (intercalate)

sieve :: Integral a => a -> [a]
sieve s = sieve' [2..s]
  where
    sieve' []       = []
    sieve' (y : ys) = y : (sieve' . filter (\l -> l `mod` y /= 0) $ ys)

findSums :: Int -> Maybe (Int, [Int])
findSums goal = findSums' goal 1000 [] (reverse $ sieve 100)

findSums' :: Int -> Int -> [Int] -> [Int] -> Maybe (Int, [Int])
findSums' goal minimumLength current primes
  | goal == 0   = Just (length current, current)
  | null primes = Nothing
  | goal < 0    = Nothing
  | length current + (goal `div` head primes) >= minimumLength = Nothing
  | otherwise   =
      let a = findSums' (goal - head primes) minimumLength (head primes : current) primes
          minimumLength' = maybe minimumLength fst a
          b = findSums' goal minimumLength' current (tail primes)
      in b `mplus` a

main :: IO ()
main = do
  cases <- readLn
  replicateM_ cases $ do
    x <- readLn
    let Just (_, ps) = findSums x
    putStrLn $ intercalate "+" $ map show $ reverse ps
