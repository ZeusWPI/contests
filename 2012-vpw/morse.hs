import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

type Morse a = State (Map (Char, Integer) Integer) a

enc :: Char -> String
enc '.' = "-."
enc '-' = "...-"
enc _   = ""

solve :: Integer -> Char -> Morse Integer
solve 1 c = return $ fromIntegral $ length (enc c)
solve n c = do
    cache <- get
    case M.lookup (c, n) cache of
        Just i -> return i
        _      -> do
            sum' <- sum <$> mapM (solve (n - 1)) (enc c)
            modify (M.insert (c, n) sum')
            return sum'

runSolve :: Integer -> String -> Integer
runSolve n str = evalState (sum <$> mapM (solve n) str) M.empty

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [w, n] <- words <$> getLine
        print $ runSolve (read n) w
