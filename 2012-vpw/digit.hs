import Control.Monad (replicateM_)
import Data.Char (digitToInt)

skipsearch :: Int  -- ^ k
           -> Int  -- ^ Number of digits skipped
           -> Int  -- ^ All numbers this wide have been skipped
           -> Int  -- ^ Requested digit
skipsearch k skipped width
    -- Not in the current range
    | skipped + skip <= k = skipsearch k (skipped + skip) width'
    -- In the current range
    | otherwise           =
        let (nth, d) = (k - skipped - 1) `divMod` width'
        in digitToInt $ show (offset + nth) !! d
  where
    offset = 10 ^ width                       -- Current range starts here
    width' = width + 1                        -- Current width
    skip   = width' * (10 ^ width' - offset)  -- Digits se'll try to skip

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        k <- readLn
        print $ skipsearch k 0 0
