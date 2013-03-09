--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
import           Control.Arrow ((***))
import           Control.Monad (replicateM, replicateM_)
import           Data.List     (partition)
import           Data.Set      (Set)
import qualified Data.Set      as S


--------------------------------------------------------------------------------
data Color = Red | Blue
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Tile = Tile {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
type Surface = Set Tile


--------------------------------------------------------------------------------
makeSurface :: Int -> Int -> Int -> Int -> Surface
makeSurface x y w h = S.fromList
    [Tile x' y' | x' <- [x .. x + w - 1], y' <- [y .. y + h - 1]]


--------------------------------------------------------------------------------
purples :: [(Color, Surface)] -> Surface
purples =
    uncurry S.intersection .
    (concat' *** concat') .
    partition ((== Red) . fst)
  where
    concat' = S.unions . map snd


--------------------------------------------------------------------------------
main :: IO ()
main = do
    testCases <- readLn
    replicateM_ testCases $ do
        nSheets <- readLn
        sheets  <- replicateM nSheets $ do
            line <- getLine
            let col          = if head line == 'R' then Red else Blue
                [x, y, w, h] = map read $ words $ tail line
            return (col, makeSurface x y w h)
        print $ S.size $ purples sheets
