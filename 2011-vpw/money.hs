import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Data.Array (Array)
import qualified Data.Array as A

--------------------------------------------------------------------------------

makeMatrix :: Int -> Int -> (Int -> Int -> a) -> Array (Int, Int) a
makeMatrix rows cols f = A.array ((0, 0), (rows - 1, cols - 1))
    [ ((r, c), f r c)
    | r <- [0 .. rows - 1]
    , c <- [0 .. cols - 1]
    ]

bottomRight :: Array (Int, Int) a -> a
bottomRight m = m A.! snd (A.bounds m)

--------------------------------------------------------------------------------

type Amount = Int
type Coins = Array Int Amount

parse :: String -> (Amount, Coins)
parse str = case map read (words str) of
    (x : xs) -> (x, A.listArray (0, length xs - 1) xs)
    _        -> error "parse"

matrix :: Amount -> Coins -> Array (Int, Int) Int
matrix amount coins = matrix'
  where
    matrix'        = makeMatrix (amount + 1) (snd (A.bounds coins) + 1) elemAt
    elemAt amt 0 = if amt /? coins A.! 0 then 1 else 0
    elemAt amt i = sum
        [matrix' A.! (a, i - 1) | a <- [0 .. amt], (a - amt) /? coins A.! i]

(/?) :: Int -> Int -> Bool
x /? y = x `mod` y == 0
infixl 7 /?

--------------------------------------------------------------------------------

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        (amount, coins) <- parse <$> getLine
        print $ bottomRight $ matrix amount coins
