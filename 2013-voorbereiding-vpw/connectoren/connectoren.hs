--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (replicateM, replicateM_)
import           Data.Map            (Map)
import qualified Data.Map            as M
import qualified Data.Set            as S


--------------------------------------------------------------------------------
data Dir = U | R | D | L
    deriving (Eq, Show)


--------------------------------------------------------------------------------
data Node = Empty | Straight | Turn
    deriving (Show)


--------------------------------------------------------------------------------
data Tile = Tile {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
type Grid = Map Tile Node


--------------------------------------------------------------------------------
parseGrid :: Int -> Int -> IO Grid
parseGrid _width height = do
    rows <- replicateM height getLine
    return $ M.fromList $
        [ (Tile x y, parseNode str)
        | (y, row) <- zip [0 ..] rows
        , (x, str) <- zip [0 ..] (words row)
        ]
  where
    parseNode "0" = Empty
    parseNode "1" = Straight
    parseNode "2" = Turn
    parseNode x   = error $ "Unknown node: " ++ x


--------------------------------------------------------------------------------
step :: Tile -> Dir -> Tile
step (Tile x y) dir = case dir of
    U -> Tile x (y - 1)
    R -> Tile (x + 1) y
    D -> Tile x (y + 1)
    L -> Tile (x - 1) y


--------------------------------------------------------------------------------
dirs :: Node -> Dir -> [Dir]
dirs Empty    _ = []
dirs Straight d = [d]
dirs Turn     U = [R, L]
dirs Turn     R = [U, D]
dirs Turn     D = [R, L]
dirs Turn     L = [U, D]


--------------------------------------------------------------------------------
follow :: (Tile -> Bool) -> Grid -> Tile -> Dir -> Bool
follow goal grid = go S.empty
  where
    go visited tile dir
        | goal tile               = True
        | tile `S.member` visited = False
        | otherwise               = case M.lookup tile grid of
            Nothing   -> False
            Just node -> or
                [ go (S.insert tile visited) (step tile dir') dir'
                | dir' <- dirs node dir
                ]


--------------------------------------------------------------------------------
leftToRight :: Int -> Int -> Grid -> Bool
leftToRight width height grid = or
    [ follow isRight grid (Tile 0 y) R
    | y <- [0 .. height - 1]
    ]
  where
    isRight (Tile x _) = x >= width


--------------------------------------------------------------------------------
main :: IO ()
main = do
    testCases <- readLn
    replicateM_ testCases $ do
        [h, w] <- map read . words <$> getLine
        grid   <- parseGrid w h
        if leftToRight w h grid
            then putStrLn "ja"
            else putStrLn "neen"
