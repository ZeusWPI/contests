{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Strict
import System.IO

data Cost
    = Cost {-# UNPACK #-} !Int
    | Impossibru
    deriving (Eq, Ord, Show)

type Cache = Map Int Cost

initialCache :: [Int] -> Cache
initialCache digits = M.fromList
    [ (read str, Cost i)
    | d <- digits
    , i <- [1 .. 10]
    , let str = concat (replicate i (show d))
    ]

type Cached a = State Cache a

muls :: Int -> [(Int, Int)]
muls n = go 2
  where
    go i
        | i * i > n      = []
        | n `mod` i == 0 = (i, n `div` i) : go (i + 1)
        | otherwise      = go (i + 1)

adds :: Int -> [(Int, Int)]
adds n = [(x, n - x) | x <- [1 .. n `div` 2]]

cost :: Int -> Cached Cost
cost n = do
    cache <- get
    case M.lookup n cache of
        Just mc -> return mc
        Nothing -> do
            let pairs            = muls n ++ adds n
                step cmin (x, y) = do
                    cx <- cost x
                    case cx of
                        Impossibru -> return cmin
                        Cost cx'
                            | (Cost $ cx' + 1) >= cmin -> return cmin
                            | otherwise                -> do
                                cy <- cost y
                                case cy of
                                    Impossibru -> return cmin
                                    Cost cy'   -> return $ min cmin (Cost $ cx' + cy')

            cost' <- foldM step Impossibru pairs
            modify (M.insert n cost')
            return cost'

testCase :: IO ()
testCase = do
    line <- getLine
    let ws         = map read $ words line
        digits     = tail (init ws)
        goal       = last ws
        (cost', _) = runState (cost goal) (initialCache digits)
    case cost' of
        Cost n     -> print n
        Impossibru -> print 0
    hFlush stdout

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases testCase
