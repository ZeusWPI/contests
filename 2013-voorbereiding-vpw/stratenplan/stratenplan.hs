--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (msum, replicateM, replicateM_)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, maybeToList)


--------------------------------------------------------------------------------
data Dir = U | R | D | L
    deriving (Eq, Show)


--------------------------------------------------------------------------------
data Tile = Node Char | Line [Dir] | Empty | Crossing
    deriving (Show)


--------------------------------------------------------------------------------
type Grid = Map (Int, Int) Tile


--------------------------------------------------------------------------------
parseGrid :: Int -> Int -> IO Grid
parseGrid _width height = do
    rows <- replicateM height getLine
    return $ M.fromList $
        [ ((x, y), parseTile c)
        | (y, row) <- zip [0 ..] rows
        , (x, c)   <- zip [0 ..] row
        ]
  where
    parseTile ' ' = Empty
    parseTile '+' = Crossing
    parseTile '-' = Line [L, R]
    parseTile '|' = Line [U, D]
    parseTile c   = Node c


--------------------------------------------------------------------------------
step :: (Int, Int) -> Dir -> (Int, Int)
step (x, y) dir = case dir of
    U -> (x, y - 1)
    R -> (x + 1, y)
    D -> (x, y + 1)
    L -> (x - 1, y)


--------------------------------------------------------------------------------
-- | What directions do we want to go in if we reach a crossing? Ordered by
-- preference.
cross :: Dir -> [Dir]
cross U = [U, L, R]
cross R = [R, U, D]
cross D = [D, L, R]
cross L = [L, U, D]


--------------------------------------------------------------------------------
follow :: (Int, Int) -> Dir -> Grid -> Maybe Char
follow xy dir grid = case M.lookup xy grid of
    Nothing               -> Nothing
    Just Empty            -> Nothing
    Just (Node c)         -> Just c
    Just (Line dirs)
        | dir `elem` dirs -> follow (step xy dir) dir grid
        | otherwise       -> Nothing
    Just Crossing         -> msum
        [ follow (step xy dir') dir' grid
        | dir' <- cross dir
        ]


--------------------------------------------------------------------------------
buildDictionary :: Grid -> Map Char Char
buildDictionary grid = M.fromList
    [ (c1, c2)
    | (xy, Node c1) <- M.toList grid
    , dir           <- [U, R, D, L]
    , c2            <- maybeToList $ follow (step xy dir) dir grid
    ]


--------------------------------------------------------------------------------
main :: IO ()
main = do
    testCases <- readLn
    replicateM_ testCases $ do
        [h, w, word] <- words <$> getLine
        grid         <- parseGrid (read w) (read h)
        let dict = buildDictionary grid
        putStrLn $ map (fromMaybe '?' . flip M.lookup dict) word
