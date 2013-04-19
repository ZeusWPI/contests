--------------------------------------------------------------------------------
import           Control.Monad       (replicateM_, forM)
import           Control.Monad.State (State, evalState, get, modify)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Vector         (Vector)
import qualified Data.Vector         as V


--------------------------------------------------------------------------------
type Foodz = Int


--------------------------------------------------------------------------------
newtype Pizza = Pizza (Vector Foodz)


--------------------------------------------------------------------------------
fromSlices :: [Foodz] -> Pizza
fromSlices = Pizza . V.fromList


--------------------------------------------------------------------------------
newtype Slice = Slice Int
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
pizzaSlices :: Pizza -> [Slice]
pizzaSlices (Pizza v) = [Slice i | i <- [0 .. V.length v - 1]]


--------------------------------------------------------------------------------
eatSlice :: Pizza -> Slice -> Foodz
eatSlice (Pizza v) (Slice i) = v V.! i


--------------------------------------------------------------------------------
nextSlice :: Pizza -> Slice -> Slice
nextSlice (Pizza v) (Slice i) =
    if i + 1 >= V.length v then Slice 0 else Slice (i + 1)


--------------------------------------------------------------------------------
previousSlice :: Pizza -> Slice -> Slice
previousSlice (Pizza v) (Slice i) =
    if i - 1 < 0 then Slice (V.length v - 1) else Slice (i - 1)


--------------------------------------------------------------------------------
-- | I start picking from slice a to slice b, I get x foodz and my mate gets y
type Cache = Map (Slice, Slice) (Foodz, Foodz)


--------------------------------------------------------------------------------
type Cached = State Cache


--------------------------------------------------------------------------------
foodzFromTo :: Pizza -> (Slice, Slice) -> Cached (Foodz, Foodz)
foodzFromTo pizza (s1, s2)
    | s1 == s2  = return (eatSlice pizza s1, 0)
    | otherwise = do
        cache <- get
        case M.lookup (s1, s2) cache of
            Just cached -> return cached
            Nothing     -> do
                eat1 <- do
                    (mate, me) <- foodzFromTo pizza (nextSlice pizza s1, s2)
                    return (eatSlice pizza s1 + me, mate)
                eat2 <- do
                    (mate, me) <- foodzFromTo pizza (s1, previousSlice pizza s2)
                    return (eatSlice pizza s2 + me, mate)
                let best = max eat1 eat2  -- Compares by fst anyway
                modify $ M.insert (s1, s2) best
                return best


--------------------------------------------------------------------------------
foodz :: Pizza -> Cached Foodz
foodz pizza = fmap maximum $ forM (pizzaSlices pizza) $ \s -> do
    (_, me) <- foodzFromTo pizza (nextSlice pizza s, previousSlice pizza s)
    return (me + eatSlice pizza s)


--------------------------------------------------------------------------------
testCase :: IO ()
testCase = do
    line <- getLine
    let slices = map read $ tail $ words line
        pizza  = fromSlices slices
        foodz' = evalState (foodz pizza) M.empty
    print foodz'


--------------------------------------------------------------------------------
main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases testCase
