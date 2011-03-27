{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as S
import Control.Arrow ((***))
import Control.Monad (replicateM_)
import Data.Maybe (fromMaybe)

powers :: Int -> Set Int
powers n = S.fromList $ 0 : powers' 1
  where
    powers' x | x > n     = [x]
              | otherwise = x : powers' (x * 2)

distanceToPower :: Set Int -> Int -> Int
distanceToPower set x =
    let (smaller, larger) = (S.findMax *** S.findMin) $ S.split x set
    in min (x - smaller) (larger - x)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = isPrime' 2
  where
    isPrime' !d | d * d > n      = True
                | n `rem` d == 0 = False
                | otherwise      = isPrime' (d + 1)

closest :: Int -> Int -> Maybe Int
closest lower upper = closest' lower Nothing 0
  where
    powers' = powers (upper * 2)

    closest' !x !best !distance
        | x > upper = best
        | isPrime x && isBetter = closest' (x + 1) (Just x) distance'
        | otherwise = closest' (x + 1) best distance
      where
        distance' = distanceToPower powers' x
        isBetter = case best of Nothing -> True
                                Just _  -> distance' < distance

main :: IO ()
main = do
    cases <- read <$> getLine
    replicateM_ cases $ do
        [m, n] <- map read . words <$> getLine
        putStrLn $ fromMaybe "geen priemgetal gevonden" $ show <$> closest m n
