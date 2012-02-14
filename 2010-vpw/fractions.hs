import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

type Product = Map Char Int

parseProduct :: String -> Product
parseProduct = foldl' (\m c -> M.insertWith' (+) c 1 m) M.empty

unparseProduct :: Product -> String
unparseProduct p = case [x | (c, n) <- M.toAscList p, x <- replicate n c] of
    []  -> "1"
    str -> str

simplify :: (Product, Product) -> (Product, Product)
simplify fraction = foldl' step fraction (M.keys $ fst fraction)
  where
    step (num, denom) c =
        let x = fromMaybe 0 $ M.lookup c num
            y = fromMaybe 0 $ M.lookup c denom
            d = min x y
        in d `seq` (M.insert c (x - d) num, M.insert c (y - d) denom)

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [num, denom] <- map parseProduct . words <$> getLine
        let (num', denom') = simplify (num, denom)
        putStrLn $ unparseProduct num' ++ " " ++ unparseProduct denom'
