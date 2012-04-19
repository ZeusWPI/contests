import Control.Applicative ((<$>))
import Control.Arrow ((>>>), (***))
import Control.Monad (replicateM_)
import Data.Ord (comparing)
import Text.Printf (printf)

type Student = (Int, Int)
data Pair    = Pair {unPair :: (Student, Student)} deriving (Eq)

instance Ord Pair where
    compare = comparing diff

diff :: Pair -> Int
diff = unPair >>> snd *** snd >>> uncurry (-) >>> abs

cheated :: [Student] -> Maybe Pair
cheated students = case map Pair (zip students $ drop 1 students) of
    []    -> Nothing
    pairs -> Just $ minimum pairs 

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        scores <- map read . drop 1 . words <$> getLine
        putStrLn $ case cheated (zip [1 ..] scores) of
            Nothing             -> "spieken kon niet"
            Just pair@(Pair ((s1, _), (s2, _)))
                | diff pair > 0 -> printf "%d en %d zijn verdacht" s1 s2
                | otherwise     -> printf "%d en %d zijn zwaar verdacht" s1 s2
