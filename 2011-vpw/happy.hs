import Control.Applicative ((<$>))
import Control.Monad (forM_, replicateM)
import Control.Monad.State (State, get, modify, evalState)
import Data.Char (ord)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type HappyCache = State (Map Int Bool)

digits :: Int -> [Int]
digits = map (\x -> ord x - ord '0') . show

step :: Int -> Int
step = sum . map (\x -> x * x) . digits

happy :: Int -> HappyCache Bool
happy x = happy' x S.empty

happy' :: Int -> Set Int -> HappyCache Bool
happy' 1   _               = return True
happy' x visited 
    | x `S.member` visited = return False
    | otherwise            = do
        cached <- (M.lookup x) <$> get
        case cached of
            Just h  -> return h
            Nothing -> do
                h <- happy' (step x) (S.insert x visited)
                modify $ M.insert x h
                return h

main :: IO ()
main = do
    cases   <- readLn
    numbers <- replicateM cases readLn
    forM_ (evalState (mapM happy numbers) M.empty) $ \x -> putStrLn $
        if x then "JA" else "NEE"
