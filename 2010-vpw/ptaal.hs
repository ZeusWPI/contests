import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Data.Char (toLower)


isVowel :: String -> Bool
isVowel st = (fmap toLower st) `elem` ["a","e","i","o","u","ij"]

decode :: String -> String
decode str = decode' str 0

decode' (x:y:xs) i
    | isVowel [x,y] = x : y : decode' xs (i+2)
    | isVowel [x]   = x :     decode' (y:xs) (i+1)
    | otherwise     = let rest = drop i (y:xs)
                      in if i == 0
                         then x : decode' rest 0
                         else     decode' rest 0
decode' x _ = x

main = do
    n <- read <$> getLine
    replicateM_ n $ do
        line <- getLine
        putStrLn $ unwords $ fmap decode $ words line
