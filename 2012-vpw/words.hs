--------------------------------------------------------------------------------
import           Control.Monad (forM_, msum, replicateM, replicateM_)
import           Data.Char     (toLower, toUpper)
import           Data.List     (intercalate)
import           Data.Set      (Set)
import qualified Data.Set      as S


--------------------------------------------------------------------------------
makeWord :: Set String -> String -> Maybe [String]
makeWord available = go []
  where
    go used "" = Just (reverse used)
    go used xs = msum
        [ go (token : used) (drop i xs)
        | i <- [1 .. length xs]
        , let token = take i xs
        , token `S.member` available
        ]


--------------------------------------------------------------------------------
showTokens :: [String] -> String
showTokens = intercalate "-" . map capitalize
  where
    capitalize []       = []
    capitalize (x : xs) = toUpper x : xs


--------------------------------------------------------------------------------
testCase :: IO ()
testCase = do
    nwords  <- readLn
    words'  <- replicateM nwords getLine
    ntokens <- readLn
    tokens  <- replicateM ntokens getLine
    let tokenSet = S.fromList $ map (map toLower) tokens
    forM_ words' $ \word -> case makeWord tokenSet word of
        Nothing       -> putStrLn "nee"
        Just solution -> putStrLn $ showTokens solution


--------------------------------------------------------------------------------
main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases testCase
