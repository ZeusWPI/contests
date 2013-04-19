--------------------------------------------------------------------------------
import           Control.Monad (replicateM_)


--------------------------------------------------------------------------------
fact :: Int -> Integer
fact n = product [1 .. fromIntegral n]


--------------------------------------------------------------------------------
main :: IO ()
main = do
    n <- readLn
    replicateM_ n $ readLn >>= print . fact