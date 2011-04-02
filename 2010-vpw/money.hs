import Data.Set (Set)
import qualified Data.Set as S
import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM_, replicateM)

type Coin = Char
data Weight = Heavier | Lighter
data Fake = Fake Coin Weight
data Measurement = Measurement (Set Coin) (Set Coin) Ordering

valid :: Measurement -> Fake -> Bool
valid (Measurement l r EQ) (Fake c _) = c `S.notMember` l && c `S.notMember` r
valid (Measurement l _ LT) (Fake c Heavier) = c `S.member` l
valid (Measurement _ r LT) (Fake c Lighter) = c `S.member` r
valid (Measurement _ r GT) (Fake c Heavier) = c `S.member` r
valid (Measurement l _ GT) (Fake c Lighter) = c `S.member` l

parseMeasurement :: String -> Measurement
parseMeasurement str =
    let [l, r, result] = words str
    in Measurement (S.fromList l) (S.fromList r) (parseOrdering result)
  where
    parseOrdering "omhoog" = LT
    parseOrdering "omlaag" = GT
    parseOrdering _        = EQ

solutions :: [Measurement] -> [Fake]
solutions = foldr (filter . valid) $
    Fake <$> ['a' .. 'z'] <*> [Heavier, Lighter]

main :: IO ()
main = do
    cases <- read <$> getLine
    replicateM_ cases $ do
        n <- read <$> getLine
        measurements <- map parseMeasurement <$> replicateM n getLine
        let solutions' = solutions measurements
        putStrLn $ case solutions' of
            []               -> "Inconsistente gegevens."
            [Fake c Heavier] -> "Het valse geldstuk " ++ [c] ++ " is zwaarder."
            [Fake c Lighter] -> "Het valse geldstuk " ++ [c] ++ " is lichter."
            _                -> "Te weinig gegevens."
