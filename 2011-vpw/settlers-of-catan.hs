import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad (replicateM_)
import Data.List (foldl', minimumBy)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Map as M

freqs :: Ord a => [a] -> Map a Int
freqs = foldl' (\m x -> M.insertWith' (+) x 1 m) M.empty

-- | E.g. 6 for a regular dice
type Dice = Int

-- | Enumerate all possible throws for a set of dices
throws :: [Dice] -> [Int]
throws []       = [0]
throws (d : ds) = [x + y | x <- [1 .. d], y <- throws ds]

-- | Find the probability a given number is thrown with a given set of dices.
-- You should preferably instantiate the first argument and then use the
-- resulting function a number of times.
probability :: [Dice] -> Int -> Double
probability dices =
    (/ possibilities) . fromIntegral . fromMaybe 0 . flip M.lookup freqs'
  where
    freqs'        = freqs $ throws dices
    possibilities = fromIntegral $ product dices

type Tile = Char

-- | Actual solution to the problem
catan :: [(Tile, Int)] -> [(Tile, Double)]
catan = map (second (sum . map prob)) .
    M.toList . M.fromListWith (++) . map (second return)
  where
    prob = probability [6, 6]

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        _       <- getLine
        tiles   <- map head . words <$> getLine
        numbers <- map read . words <$> getLine

        let probs = catan $ zip tiles numbers
            least = fst $ minimumBy (comparing snd) probs

        putStrLn [least]
