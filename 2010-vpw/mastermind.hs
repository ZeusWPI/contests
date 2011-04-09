{-# LANGUAGE BangPatterns #-}
import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Control.Arrow ((***), (>>>))
import Data.Char (digitToInt, intToDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

type Color = Int
type Code = [Color]

data Guess = Guess Code (Int, Int)
           deriving (Show, Eq)        

-- | Lazily generate possible solutions
--
generate :: Int -> Int -> [Code]
generate 0 _ = [[]]
generate n k = [ color : rest | color <- [1 .. k], rest <- generate (n - 1) k ]

-- | Generate an answer for a given code and guess. Returns a @(b, w)@ tuple
-- where @b@ is the number of correct pins, and @w@ the number of pins in the
-- correct color
--
answer :: Code -> Code -> (Int, Int)
answer = answer' 0 IM.empty []
  where
    answer' :: Int         -- ^ Current correct
            -> IntMap Int  -- ^ Missed
            -> [Color]     -- ^ Left over
            -> Code        -- ^ Expected
            -> Code        -- ^ Guess
            -> (Int, Int)  -- ^ Result
    answer' !c m l (e : es) (x : xs)
        | e == x    = answer' (c + 1) m l es xs
        | otherwise = answer' c (IM.insertWith (+) e 1 m) (x : l) es xs
    answer' !c m l _ _ = (c, missed m l 0)

    missed _ [] !c       = c
    missed m (l : ls) !c = case IM.lookup l m of
        Nothing -> missed m ls c
        Just 0  -> missed m ls c
        Just n  -> missed (IM.insert l (n - 1) m) ls (c + 1)

-- | Find the set of codes matching the given set of matches
--
codes :: Int -> Int -> [Guess] -> [Code]
codes n k = foldr (filter . matches) (generate n k)
  where
    matches (Guess code answer') = (== answer') . answer code

-- | Parse a guess
--
parseGuess :: String -> Guess
parseGuess = break (== ':') >>> parseCode *** parseAnswer >>> uncurry Guess
  where
    parseCode = map digitToInt
    parseAnswer (':' : b : ',' : w : []) = (digitToInt b, digitToInt w)
    parseAnswer _                        = error "parseGuess: incorrect format"

main :: IO ()
main = do
    cases <- read <$> getLine
    replicateM_ cases $ do
        [n, k, _] <- map read . words <$> getLine
        guesses <- map parseGuess . words <$> getLine
        putStrLn $ unparse $ codes n k guesses
    return ()
  where
    unparse [] = "0"
    unparse c  = show (length c) ++ " " ++ unwords (map (map intToDigit) c)
