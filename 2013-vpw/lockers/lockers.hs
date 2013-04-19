--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (foldM, forM, replicateM, replicateM_)
import           Control.Monad.State (State, evalState, get, modify)
import           Data.List           (sort, sortBy)
import           Data.Ord            (comparing)


--------------------------------------------------------------------------------
data Time = Time Int Int deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
addMins :: Int -> Time -> Time
addMins x (Time h m) = let (h', m') = (m + x) `divMod` 60 in Time (h + h') m'


--------------------------------------------------------------------------------
data Dim = Dim Int Int Int deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
fits :: Dim -> Dim -> Bool
fits (Dim x1 y1 z1) (Dim x2 y2 z2) =
    and $ zipWith (<=)
        (sort [x1, y1, z1])
        (sort [x2, y2, z2])


--------------------------------------------------------------------------------
type Bag = Dim


--------------------------------------------------------------------------------
data Locker = Empty Dim | Full deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data Person = Person
    { personArrival :: Time
    , personDepart  :: Time
    , personBags    :: [Bag]
    }


--------------------------------------------------------------------------------
data World = World {worldTime :: Time, worldLockers :: [Locker]} deriving (Show)


--------------------------------------------------------------------------------
tick :: Int -> State World ()
tick mins = modify $ \w -> w {worldTime = addMins mins (worldTime w)}


--------------------------------------------------------------------------------
waitUntil :: Time -> State World ()
waitUntil end = modify $ \w -> w
    { worldTime = let wt = worldTime w in if wt < end then end else wt
    }


--------------------------------------------------------------------------------
dropBags :: Person -> State World Person
dropBags person = do
    lockers           <- worldLockers <$> get
    (bags', lockers') <- foldM dropBagssAt (personBags person, []) lockers
    modify $ \w -> w {worldLockers = reverse lockers'}
    return person {personBags = bags'}
  where
    dropBagssAt ([], ls) l              = return ([], l : ls)
    dropBagssAt (bs, ls) Full           = do
        tick 1
        return (bs, Full : ls)
    dropBagssAt (bs, ls) (Empty locker) = do
        case break (`fits` locker) bs of
            -- No fit
            (bs1, [])        -> do
                tick (1 + length bs1 * 2)
                return (bs1, Empty locker : ls)
            -- If I fits, I sits
            (bs1, (_ : bs2)) -> do
                tick (1 + length bs1 * 2 + 3)
                return (bs1 ++ bs2, Full : ls)


--------------------------------------------------------------------------------
simulate :: [Person] -> State World [Bool]
simulate people = do
    forM inOrder $ \person -> do
        waitUntil $ personArrival person
        person' <- dropBags person
        let extraTime = length (personBags person') * 10
        time <- worldTime <$> get
        return $ addMins extraTime time <= personDepart person'
  where
    inOrder = sortBy (comparing personArrival) people


--------------------------------------------------------------------------------
testCase :: IO ()
testCase = do
    numLockers <- readLn
    numPeople  <- readLn
    lockers    <- map Empty <$> replicateM numLockers parseDim
    people     <- replicateM numPeople $ do
        numBags <- readLn
        arrival <- parseTime
        depart  <- parseTime
        bags    <- replicateM numBags parseDim
        return $ Person arrival depart bags
    let result = evalState (simulate people) (World (Time 0 0) lockers)
    putStrLn $ unwords $ [if r then "JA" else "NEEN" | r <- result]
  where
    parseDim = do
        [x, y, z] <- map read . words <$> getLine
        return $ Dim x y z
    parseTime = do
        [h, m] <- map read . words <$> getLine
        return $ Time h m


--------------------------------------------------------------------------------
main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases testCase
