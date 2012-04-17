import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (forM_)
import Data.Char (digitToInt, isDigit)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)

data C = C Char | I Int | D
    deriving (Show)

fromString :: String -> [C]
fromString cs = [D] ++ intersperse (I 0) (map C cs) ++ [D]

toString :: [C] -> String
toString = concatMap toString'
  where
    toString' (C x) = [x]
    toString' (I x) = if odd x then "-" else ""
    toString' _     = []

fromPattern :: String -> [C]
fromPattern = map fromChar
  where
    fromChar '.' = D
    fromChar x   = if isDigit x then I (digitToInt x) else C x

apply :: [C] -> [C] -> Maybe [C]
apply []         xs         = Just xs
apply (D   : ps) (D   : xs) = (D :) <$> apply ps xs
apply (D   : _)  (_   : _)  = Nothing
apply (C p : ps) (C x : xs)
    | p == x                = (C x :) <$> apply ps xs
    | otherwise             = Nothing
apply (I p : ps) (I x : xs)
    | p > x                 = (I p :) <$> apply ps xs
    | otherwise             = (I x :) <$> apply ps xs
apply (C p : ps) (I x : xs) = (I x :) <$> apply (C p : ps) xs
apply _          _          = Nothing

rule :: [C] -> [C] -> [C]
rule pattern []   = []
rule pattern word =
    let word' = fromMaybe word (apply pattern word)
    in head word' : rule pattern (tail word')

rules :: [[C]] -> [C] -> [C]
rules patterns word = foldr rule word patterns

main :: IO ()
main = do
    ls <- lines <$> getContents
    let (ps, ws) = map fromPattern *** drop 1 $ break (all (== '-')) ls
    forM_ ws $ putStrLn . toString . rules ps . fromString
