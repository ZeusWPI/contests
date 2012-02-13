import Control.Applicative ((<$>))
import Control.Monad (replicateM, replicateM_)
import Data.Bits (setBit, testBit)
import Data.Char (chr, ord)
import Data.List (delete, nub)
import Data.Map (Map)
import qualified Data.Map as M

data Tile
    = Tile {tileU :: Bool, tileR :: Bool, tileD :: Bool, tileL :: Bool}
    | Unknown
    deriving (Eq, Show)

parseTile :: Char -> Tile
parseTile '?' = Unknown
parseTile c   = Tile (testBit i 0) (testBit i 1) (testBit i 2) (testBit i 3)
  where
    i = ord c - ord 'A' + 1

unparseTile :: Tile -> Char
unparseTile Unknown        = '?'
unparseTile (Tile u r b l) =
    let i = sb u 0 $ sb r 1 $ sb b 2 $ sb l 3 $ 0
    in chr $ i + ord 'A' - 1
  where
    sb s i x = if s then setBit x i else x

type Field = Map (Int, Int) Tile

parseField :: [String] -> Field
parseField rows = M.fromList
    [ ((r, c), parseTile x)
    | (r, row) <- zip [0 ..] rows
    , (c, x)   <- zip [0 ..] row
    ]

unparseField :: Int -> Field -> [String]
unparseField size field = do
    r <- [0 .. size - 1]
    let tiles = [field M.! (r, c) | c <- [0 .. size - 1]]
    return $ map unparseTile tiles

unknowns :: Field -> [(Int, Int)]
unknowns = map fst . filter ((== Unknown) . snd) . M.toList

matches :: (Int, Int) -> Tile -> Field -> Bool
matches _      Unknown                   _     = True
matches (r, c) (Tile up right down left) field =
    match up    (r - 1, c) tileD &&
    match right (r, c + 1) tileL &&
    match down  (r + 1, c) tileU &&
    match left  (r, c - 1) tileR
  where
    match x p f = case M.lookup p field of
        Nothing      -> not x  -- Outside of grid
        Just Unknown -> True
        Just t       -> x == f t

solve :: Int -> Field -> [(Int, Int)] -> [Tile] -> [Field]
solve _    field []       _         = [field]
solve size field (p : ps) available =
    [ solution
    | tile <- nub available
    , matches p tile field
    , let field'     = M.insert p tile field
    , let available' = delete tile available
    , solution <- solve size field' ps available'
    ]

main :: IO ()
main = do
    n <- readLn
    replicateM_ n $ do
        size      <- readLn
        available <- map parseTile <$> getLine
        field     <- parseField <$> replicateM size getLine
        case solve size field (unknowns field) available of
            [x] -> putStr $ unlines $ unparseField size x
            _   -> error "No single solution found"
