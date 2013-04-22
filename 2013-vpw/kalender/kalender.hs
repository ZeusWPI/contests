--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
import           Control.Monad (replicateM, replicateM_)
import           Data.List     (transpose)


--------------------------------------------------------------------------------
findRepeated :: Eq a => a -> Int -> [a] -> Maybe Int
findRepeated needle needed = go 0 0
  where
    go _    _    []         = Nothing
    go !idx !num (x : xs)
        | x /= needle       = go (idx + num + 1) 0 xs
        | num + 1 >= needed = Just idx
        | otherwise         = go idx (num + 1) xs


--------------------------------------------------------------------------------
testCase :: IO ()
testCase = do
    numPeople <- readLn
    _numDays  <- readLn :: IO Int
    holiday   <- readLn :: IO Int
    calendar  <- replicateM numPeople $ fmap (map (== 'V')) getLine
    let perDay = map and $ transpose calendar
    case findRepeated True holiday perDay of
        Nothing  -> putStrLn "X"
        Just idx -> print (idx + 1)


--------------------------------------------------------------------------------
main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases testCase
