import Control.Applicative (Alternative (..), Applicative (..), (<$>))
import Control.Monad (MonadPlus (..), ap, guard, replicateM, replicateM_)
import Data.Char (isDigit, ord)
import Data.List (find)
import Data.Map (Map)
import Data.Maybe (isNothing, maybeToList)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)

--------------------------------------------------------------------------------

newtype Parser a = Parser {unParser :: String -> [(a, String)]}

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance MonadPlus Parser where
    mzero                     = Parser $ const []
    Parser x `mplus` Parser y = Parser $ \str -> x str ++ y str

instance Functor Parser where
    fmap f (Parser x) = Parser $ \str -> [(f x', str') | (x', str') <- x str]

instance Monad Parser where
    return x       = Parser $ \str -> [(x, str)]
    Parser x >>= f = Parser $ \str ->
        [ (x'', str'')
        | (x', str')   <- x str
        , (x'', str'') <- unParser (f x') str'
        ]

runParser :: Parser a -> String -> Maybe a
runParser (Parser x) = fmap fst . find (null . snd) . x

item :: Parser Char
item = Parser $ \str -> case str of
    (x : xs) -> [(x, xs)]
    []       -> []

char :: Char -> Parser Char
char c = do
    x <- item
    guard $ c == x
    return x

digit :: Parser Int
digit = do
    x <- item
    guard $ isDigit x
    return $ ord x - ord '0'

number :: Parser Int
number = do
    digits <- many digit
    return $ foldl (\x d -> 10 * x + d) 0 digits

--------------------------------------------------------------------------------

data Value
    = Bool Bool
    | Int  Int
    deriving (Eq, Show)

data Expression
    = Literal Value
    | Equals  Expression Expression
    | Plus    Expression Expression
    | Minus   Expression Expression
    | Times   Expression Expression
    deriving (Show)

value :: Parser Value
value = Int <$> number

expression :: Parser Expression
expression = exp1
  where
    exp1 =
        binop '=' Equals exp2 exp1 <|>
        exp2

    exp2 =
        binop '+' Plus  exp3 exp2 <|>
        binop '-' Minus exp3 exp2 <|>
        exp3

    exp3 =
        binop 'x' Times exp4 exp3 <|>
        exp4

    exp4 = Literal <$> value

    binop o cons e1 e2 = do
        x <- e1
        _ <- char o
        y <- e2
        return $ cons x y

eval :: Expression -> Value
eval e = case e of
    Literal val  -> val
    Equals e1 e2 -> Bool $ eval e1 == eval e2
    Plus   e1 e2 -> binop (+) e1 e2
    Minus  e1 e2 -> binop (-) e1 e2
    Times  e1 e2 -> binop (*) e1 e2
  where
    binop o e1 e2 =
        let Int x = eval e1
            Int y = eval e2
        in Int $ o x y

--------------------------------------------------------------------------------

type Position = (Int, Int)

type Grid = Map (Int, Int) Char

parseGrid :: [String] -> Grid
parseGrid rows = M.fromList
    [ ((r, c), x)
    | (r, row) <- zip [0 ..] rows
    , (c, x)   <- zip [0 ..] row
    ]

neighbours :: Position -> [Position]
neighbours (r, c) = [(r - 1, c), (r, c + 1), (r + 1, c), (r, c - 1)]

paths :: Grid -> [String]
paths grid = [path | start <- M.keys grid, path <- paths' start "" S.empty]
  where
    paths' pos path visited
        | pos `M.notMember` grid            = []
        | pos `S.member` visited            = []
        | S.size visited + 1 >= M.size grid = [path']
        | bound                             = []
        | otherwise                         =
            [ p
            | neighbour <- neighbours pos
            , p         <- paths' neighbour path' (S.insert pos visited)
            ]
      where
        path' = grid M.! pos : path
        bound = case path' of
            (x : y : _)
                | not (isDigit x || isDigit y) -> True
                | otherwise                    -> False
            [x]                                -> not (isDigit x)
            _                                  -> False

solutions :: Grid -> [String]
solutions grid =
    [ path
    | path <- paths grid
    , e    <- maybeToList $ runParser expression path
    , eval e == Bool True
    ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [rows, _] <- map read . words <$> getLine
        grid      <- parseGrid <$> replicateM rows getLine
        case solutions grid of
            (x : _) -> putStrLn x
            _       -> putStrLn "No solution"
