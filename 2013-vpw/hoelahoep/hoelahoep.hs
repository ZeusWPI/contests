--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
import           Control.Applicative ((<$>))
import           Control.Monad       (replicateM, replicateM_)
import           Data.List           (foldl')
import           Data.Maybe          (maybeToList)
import           Data.Set            (Set)
import qualified Data.Set            as S


--------------------------------------------------------------------------------
type Connector = Int


--------------------------------------------------------------------------------
data Segment = Segment {-# UNPACK #-} !Connector {-# UNPACK #-} !Connector
    deriving (Eq, Show, Ord)


--------------------------------------------------------------------------------
type Hulahoop = Segment


--------------------------------------------------------------------------------
type Box = [Segment]


--------------------------------------------------------------------------------
canClose :: Hulahoop -> Bool
canClose (Segment x y) = x == y


--------------------------------------------------------------------------------
appendSegment :: Segment -> Hulahoop -> Maybe Hulahoop
appendSegment (Segment x y) (Segment s e)
    | x == e    = Just $ Segment s y
    | y == e    = Just $ Segment s x
    | otherwise = Nothing


--------------------------------------------------------------------------------
-- | Creates initial 'Hulahoop's from the first box
fromBox :: Box -> Set Hulahoop
fromBox box = S.fromList
    [Segment s e | Segment x y  <- box, (s, e) <- [(x, y), (y, x)]]


--------------------------------------------------------------------------------
-- | Append the segments in the box in all possible ways
appendFromBox :: Box -> Set Hulahoop -> Set Hulahoop
appendFromBox box hulahoops = S.fromList
    [ hulahoop'
    | hulahoop  <- S.toList hulahoops
    , segment   <- box
    , hulahoop' <- maybeToList $ appendSegment segment hulahoop
    ]


--------------------------------------------------------------------------------
longest :: [Box] -> Int
longest []                 = 0
longest (firstBox : boxes) =
    let hulahoops     = fromBox firstBox
        (_, long', _) = foldl' go (1, long, hulahoops) boxes
        long          = if any canClose (S.toList hulahoops) then 1 else 0
    in long'
  where
    go :: (Int, Int, Set Hulahoop) -> Box -> (Int, Int, Set Hulahoop)
    go (!len, !long, !hulahoops) box =
        let hulahoops' = appendFromBox box hulahoops
            closable   = any canClose (S.toList hulahoops')
            long'      = if closable then len' else long
            len'       = len + 1
        in (len', long', hulahoops')


--------------------------------------------------------------------------------
testCase :: IO ()
testCase = do
    numBoxes    <- readLn
    numSegments <- readLn
    boxes       <- replicateM numBoxes $ replicateM numSegments readSegment
    print $ longest boxes
  where
    readSegment = do
        [x, y] <- map read. words <$> getLine
        return $ Segment x y


--------------------------------------------------------------------------------
main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases testCase
