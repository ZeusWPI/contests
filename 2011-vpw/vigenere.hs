-- Solution to http://www.vlaamseprogrammeerwedstrijd.be/2011/opgaves/cat1-2011/vignere.pdf
import Data.Char
import Control.Monad
import Control.Applicative

vigenere :: (Int -> Int -> Int) -> String -> String -> String
vigenere (<>) str code = zipWith givSum str $ concat $ repeat code
  where
    givSum s c = chr' $ (ord' s <> ord' c) `mod` 27

ord' ' ' = 0
ord' l   = ord l - ord 'A' + 1

chr' 0 = ' '
chr' l = chr $ ord 'A' + l - 1

encode = vigenere (+)
decode = vigenere (-)

main = do
    [encode, decode] `forM_` \f -> do
        cases <- readLn
        replicateM_ cases $ do
            (code:str) <- words <$> getLine
            putStrLn $ f (unwords str) code
