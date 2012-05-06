import Control.Monad (replicateM, replicateM_)
import Control.Monad.RWS (RWS, ask, get, modify, runRWS, tell)
import Data.List (transpose)

type Quilt = [String]
type Machine a = RWS [Quilt] [Quilt] [Quilt] a  -- Base patterns, out, stack

turn :: Quilt -> Quilt
turn = map (map t . reverse) . transpose
  where
    t '-'  = '|'
    t '|'  = '-'
    t '/'  = '\\'
    t '\\' = '/'
    t x    = x

knit :: Quilt -> Quilt -> Quilt
knit = zipWith (++)

exec :: [String] -> Machine ()
exec []       = return ()
exec (c : cs) = case c of
    "naai"  -> modify (\(x : y : s) -> knit x y : s)      >> exec cs
    "draai" -> modify (\(x : s) -> turn x : s)            >> exec cs
    "teken" -> get >>= tell . return . head               >> exec cs
    "stop"  -> return ()
    _       -> ask >>= \b -> modify (b !! (read c - 1) :) >> exec cs

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        numBases    <- readLn
        bases       <- replicateM numBases $ replicateM 2 getLine
        numCommands <- readLn
        commands    <- replicateM numCommands getLine
        let (_, _, qs) = runRWS (exec commands) bases []
        mapM_ putStrLn (map unlines qs)
