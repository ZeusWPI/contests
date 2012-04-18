import Data.Map (Map)
import qualified Data.Map as M

type Unions a = Map a a

singletons :: Ord a => [a] -> Unions a
singletons = const M.empty

find :: Ord a => a -> Unions a -> (a, Unions a)
find x unions = case M.lookup x unions of
    Nothing         -> (x, unions)
    Just y
        | x == y    -> (x, unions)
        | otherwise -> let (r, unions') = find y unions
                       in  (r, M.insert x r unions')

isUnion :: Ord a => a -> a -> Unions a -> (Bool, Unions a)
isUnion x y unions =
    let (r1, unions')  = find x unions
        (r2, unions'') = find y unions'
    in (r1 == r2, unions'')

union :: Ord a => a -> a -> Unions a -> Unions a
union x y unions =
    let (r, unions') = find y unions
    in M.insert x r $ M.insert y r unions'
