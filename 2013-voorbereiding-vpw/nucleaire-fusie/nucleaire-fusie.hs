--------------------------------------------------------------------------------
import           Control.Applicative ((<$>))
import           Control.Monad       (replicateM, replicateM_)
import           Data.Map            (Map)
import qualified Data.Map            as M


--------------------------------------------------------------------------------
type Atom = Int


--------------------------------------------------------------------------------
type AtomNames = Map String Atom


--------------------------------------------------------------------------------
parseAtomNames :: Int -> IO AtomNames
parseAtomNames n = fmap M.fromList $ replicateM n $ do
    [name, num] <- words <$> getLine
    return (name, read num)


--------------------------------------------------------------------------------
fuse :: [Atom] -> [Atom] -> Bool
fuse = go [] 0
  where
    go _      _    []   []      = True
    go _      _    _    []      = False
    go _      _    []   _       = False
    go unused curr (x : xs) (g : gs)
        | curr > g       = False
        | otherwise      =
            (curr + x == g && go [] 0 (unused ++ xs) gs) ||
            go unused (curr + x) xs (g : gs)             ||
            go (x : unused) curr xs (g : gs)


--------------------------------------------------------------------------------
parseAtoms :: AtomNames -> IO [Atom]
parseAtoms atomNames = map fromName . words <$> getLine
  where
    fromName name = case M.lookup name atomNames of
        Just a -> a
        _      -> error $ "Unknown atom: " ++ name


--------------------------------------------------------------------------------
main :: IO ()
main = do
    numAtoms  <- readLn
    atomNames <- parseAtomNames numAtoms
    numFuses  <- readLn
    replicateM_ numFuses $ do
        available <- parseAtoms atomNames
        goal      <- parseAtoms atomNames
        if fuse available goal
            then putStrLn "ja"
            else putStrLn "neen"
