import Control.Applicative
import Control.Monad.State
import Control.Monad

import Data.List (intercalate)

zeef s = zeef' [2..s]

zeef' [] = []
zeef' (y:ys) = y : (zeef' . filter (\l -> l `mod` y /= 0) $ ys)

erasto :: [Int]
erasto = zeef 100

solve n = solve' n primes n (0,[])
  where
    primes = takeWhile (<= n) erasto

solve' :: Int -> [Int] -> Int -> (Int,[Int]) -> (Int, [Int])
solve' 0 _ minlen (len,s)        = (len, s)
solve' _ [] _ (len,s)            = (len, s)
solve' num primes minlen (len,s) =
    foldr f (minlen, s) primes'
  where
    -- only take primes lower than num
    primes' = takeWhile (<= num) primes

    minLenReq p = num `div` p

    f pr (ml,res) = if ml > len + minLenReq pr then
                        let (l',res') = solve' (num - pr) primes' ml ((len + 1),(pr:s))
                        in if l' < ml then (l', res')
                                      else (ml, res)
                    else (ml, res)


main = do
    n <- read <$> getLine
    replicateM_ n $ do
        num <- read <$> getLine
        let (_, ar) = solve num
        putStrLn $ intercalate "+" $ map show $ reverse ar




