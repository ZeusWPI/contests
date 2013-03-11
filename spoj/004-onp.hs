--------------------------------------------------------------------------------
-- | Simple shunting-yard algorithm
import           Control.Monad (replicateM_)
import           Data.Char     (isAlpha)


--------------------------------------------------------------------------------
toRpn :: String -> String
toRpn = go []


--------------------------------------------------------------------------------
go :: [Char] -> String -> String
go stack []       = stack
go stack (x : xs)
    | isAlpha x    = x : go stack xs
    | isOperator x =
        let (ops, stack') = break
                (\o -> not (isOperator o) || precedence x < precedence o)
                stack
        in ops ++ go (x : stack') xs
    | x == '('     = go ('(' : stack) xs
    | x == ')'     = case break (== '(') stack of
        (tokens, '(' : stack') -> tokens ++ go stack' xs
        _                      -> error $ "go: Corrupt stack"
    | otherwise    = error $ "go: Unknown token: " ++ show x


--------------------------------------------------------------------------------
isOperator :: Char -> Bool
isOperator = (`elem` "+-*/^")


--------------------------------------------------------------------------------
precedence :: Char -> Int
precedence '+' = 4
precedence '-' = 3
precedence '*' = 2
precedence '/' = 1
precedence '^' = 0
precedence x   = error $ "precedence: Not an operator: " ++ show x


--------------------------------------------------------------------------------
main :: IO ()
main = do
    n <- readLn
    replicateM_ n $ do
        expr <- getLine
        putStrLn $ toRpn expr
