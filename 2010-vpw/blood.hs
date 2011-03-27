module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard, replicateM_)
import Data.List (nub, sort, intercalate)
import Control.Arrow ((***), (>>>))

data Marker = A | B | O
            deriving (Show, Eq, Ord)

data Marker_ = A_ | AB_ | B_ | O_
             deriving (Show, Eq, Ord)

combineMarker :: Marker -> Marker -> Marker_
combineMarker A A = A_
combineMarker A B = AB_
combineMarker A O = A_
combineMarker B B = B_
combineMarker B O = B_
combineMarker O O = O_
combineMarker x y = combineMarker y x

decombineMarker :: Marker_ -> [(Marker, Marker)]
decombineMarker A_  = [(A, A), (A, O)]
decombineMarker AB_ = [(A, B)]
decombineMarker B_  = [(B, B), (B, O)]
decombineMarker O_  = [(O, O)]

data Rhesus = Plus | Minus
            deriving (Show, Eq, Ord)

data Rhesus_ = Plus_ | Minus_
             deriving (Show, Eq, Ord)

combineRhesus :: Rhesus -> Rhesus -> Rhesus_
combineRhesus Plus  Plus  = Plus_
combineRhesus Plus  Minus = Plus_
combineRhesus Minus Plus  = Plus_
combineRhesus Minus Minus = Minus_

decombineRhesus :: Rhesus_ -> [(Rhesus, Rhesus)]
decombineRhesus Plus_  = [(Plus, Plus), (Plus, Minus)]
decombineRhesus Minus_ = [(Minus, Minus)]

data Person = Person Marker_ Rhesus_
            deriving (Show, Eq, Ord)

children :: Person -> Person -> [Person]
children (Person m1 r1) (Person m2 r2) = nub $ sort $ do
    (m11, m12) <- decombineMarker m1 
    (r11, r12) <- decombineRhesus r1
    (m21, m22) <- decombineMarker m2
    (r21, r22) <- decombineRhesus r2
    m1' <- [m11, m12] 
    m2' <- [m21, m22] 
    r1' <- [r11, r12]
    r2' <- [r21, r22]
    return $ Person (combineMarker m1' m2') (combineRhesus r1' r2')

parent :: Person -> Person -> [Person]
parent parent1 child = nub $ sort $ do
    parent2 <- Person <$> [A_, AB_, B_, O_] <*> [Plus_, Minus_]
    guard (child `elem` children parent1 parent2) 
    return parent2

parsePerson :: String -> Person
parsePerson =
    break (`elem` "+-") >>> parseMarker *** parseRhesus >>> uncurry Person
  where
    parseMarker "A"  = A_
    parseMarker "AB" = AB_
    parseMarker "B"  = B_
    parseMarker "O"  = O_
    parseMarker _    = undefined
    parseRhesus "+" = Plus_
    parseRhesus "-" = Minus_
    parseRhesus _   = undefined

unparsePerson :: Person -> String
unparsePerson (Person m r) = unparseMarker m ++ unparseRhesus r
  where
    unparseMarker A_  = "A"
    unparseMarker AB_ = "AB"
    unparseMarker B_  = "B"
    unparseMarker O_  = "O"
    unparseRhesus Plus_  = "+"
    unparseRhesus Minus_ = "-"

main :: IO ()
main = do
    cases <- read <$> getLine
    replicateM_ cases $ do
        line <- getLine
        let solution = case words line of
                ["?", p, c]   ->
                    let p' = parent (parsePerson p) (parsePerson c)
                    in [pretty p', p, c]
                [p, "?", c]   ->
                    let p' = parent (parsePerson p) (parsePerson c)
                    in [p, pretty p', c]
                [p1, p2, "?"] ->
                    let c = children (parsePerson p1) (parsePerson p2)
                    in [p1, p2, pretty c]
                x             -> x
        putStrLn $ unwords solution
  where
    pretty persons = case length persons of
        0 -> "ONMOGELIJK"
        1 -> unparsePerson (head persons)
        _ -> "{" ++ intercalate "," (map unparsePerson persons) ++ "}"
