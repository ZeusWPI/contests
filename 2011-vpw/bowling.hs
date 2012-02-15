-- Solution to http://www.vlaamseprogrammeerwedstrijd.be/2011/opgaves/cat2-2011/bowling.pdf
import Control.Monad
import Control.Applicative

import Data.List

data Frame = Throw Int Int
           | Strike
           | Spare Int
           | Last Int Int Int
    deriving (Show)

data Score = Score Int Int
    deriving (Show)

type Game = [Frame]

parseGame :: String -> Game
parseGame str = parseThrows throws
  where
    throws = map read $ words str

parseThrows :: [Int] -> [Frame]
parseThrows (x:xs) = parseThrows' x xs 0

parseThrows' b  (x:[])   9 = [Last b x 0]
parseThrows' b  (x:y:[]) 9 = [Last b x y]
parseThrows' 10 (x:y:[]) l = Strike : parseThrows' x [y] (l+1)
parseThrows' 10 (x:xs)   l = Strike : parseThrows' x xs  (l+1)
parseThrows' buf (x:y:xs) l
    | buf + x == 10 = Spare buf : parseThrows' y xs (l+1)
    | otherwise = (Throw buf x) : parseThrows' y xs (l+1)

valid :: Game -> Bool
valid game = length game == 10 && all valid' game
  where
    valid' (Throw a b) = a + b < 10
    valid' (Spare a)   = a < 10
    valid' Strike      = True
    valid' (Last a b c)
        | a == 10     = valid' (Last b c 0)
        | a + b < 10  = c == 0
        | a + b == 10 = c <= 10
        | otherwise   = False

firstThrow :: Frame -> Int
firstThrow (Throw a _)  = a
firstThrow (Spare a)    = a
firstThrow (Last a _ _) = a
firstThrow Strike       = 10


secondThrow (Throw _ a)  = a
secondThrow (Spare a)    = 10 - a
secondThrow (Last _ a _) = a
secondThrow Strike       = 0

score :: Game -> [Score]
score ((Throw a b):n:fs) = Score (a+b) 0 : score (n:fs)
score ((Spare a):n:fs) = Score (10) (firstThrow n) : score (n:fs)
score (Strike:n:fs)  =
    case n of
        Strike -> Score (10) (10 + firstThrow (head fs)) : score (n:fs)
        _      -> Score (10) (firstThrow n + secondThrow n) : score (n:fs)
score ((Last a b c):[]) = [Score (a+b+c) 0]

scoreToInt :: [Score] -> [Int]
scoreToInt scores = snd $ mapAccumL (\acc s -> (acc + (toInt s), acc + (toInt s))) 0 scores
    where toInt (Score a b) = a + b

main = do
    cases <- readLn
    replicateM_ cases $ do
        line <- getLine
        let game = parseGame line
        if valid game then putStrLn $ intercalate " " $ map show $ scoreToInt $ score game
                      else putStrLn "ONGELDIG"
