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
