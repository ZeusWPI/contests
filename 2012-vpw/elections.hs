import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Data.List (mapAccumL)

type Votes = Int
type Seats = Int
type Party = (Votes, Seats)

assign :: Votes    -- ^ Number of votes
       -> Int      -- ^ Assignation round
       -> Seats    -- ^ Seats left to assign
       -> [Party]  -- ^ Parties
       -> [Party]  -- ^ Result
assign total round' left parties
    | needed > left = parties
    | otherwise     = assign total (round' + 1) (left - needed) parties'
  where
    (needed, parties')            = mapAccumL assign' 0 parties
    assign' n (votes, seats)
        | votes * round' >= total = (n + 1, (votes, seats + 1))
        | otherwise               = (n, (votes, seats))

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        numSeats <- readLn
        _        <- getLine
        parties  <- (`zip` repeat 0) . map read . words <$> getLine
        let total    = sum $ map fst parties
            parties' = assign total 1 numSeats parties
        putStrLn $ unwords $ map (show . snd) parties'
