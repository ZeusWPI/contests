import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

type Morse a = State (Map (Char, Integer) Integer) a

enc :: Char -> String
enc '.' = "-."
enc '-' = "...-"

solve :: Integer -> Char -> Morse Integer
solve 1 c = return $ fromIntegral $ length (enc c)
solve n c = do
    map' <- get
    case M.lookup (c, n) map' of
        Just i -> return i
        _      -> do
            let enc' = enc c
            ls <- mapM (solve (n - 1)) enc'
            let sum' = sum ls
            modify (M.insert (c, n) sum')
            return sum'

solve' :: Integer -> String -> Morse Integer
solve' n str = sum <$> mapM (solve n) str

runSolve :: Integer -> String -> Integer
runSolve n str = evalState (solve' n str) M.empty

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [w, n] <- words <$> getLine
        print $ runSolve (read n) w
