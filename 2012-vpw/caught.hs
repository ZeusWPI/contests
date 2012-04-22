import Control.Applicative ((<$>))
import Control.Monad (forM_, replicateM_)
import Data.Set (Set)
import qualified Data.Set as S

type C = Int

circle :: Int -> Int -> ([C] -> [C]) -> ([C] -> [C]) -> Bool -> [C]
       -> (Int, [C], ([C] -> [C]), Bool)
circle _ x re el e [] = (x, re [], el, e)
circle n x re el e (c : cs)
    | x == c    = circle n 1  re           (el . (c :)) True cs
    | otherwise = circle n x' (re . (c :)) el           e    cs
  where
    x' = if x >= n then 1 else x + 1

circles :: Int -> Int -> Set Int -> ([C] -> [C]) -> [C] -> ([C], [C])
circles n x xs el cs
    | not e && x `S.member` xs = (cs', el' [])
    | otherwise                = circles n x' xs' el' cs'
  where
    (x', cs', el', e) = circle n x id el False cs
    xs'               = if e then S.empty else S.insert x xs

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        (n : cs) <- map read . words <$> getLine
        let (cs', el) = circles n 1 S.empty id cs
        forM_ el $ putStrLn . (++ " gevangen") . show
        putStrLn $ if null cs'
            then "alles gevangen"
            else "er kunnen geen kaarten meer gevangen worden"
