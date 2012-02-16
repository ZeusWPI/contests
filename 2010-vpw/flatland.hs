import Control.Applicative ((<$>))
import Control.Monad (forM_, replicateM, replicateM_)

type Rectangular = (Double, Double)

type Line = (Rectangular, Rectangular)

type Polar = (Double, Double)

toPolar :: Rectangular -> Polar
toPolar (x, y) = (sqrt (x * x + y * y), atan (y / x))

fromPolar :: Polar -> Rectangular
fromPolar (r, q) = (r * cos q, r * sin q)

toDegrees :: Double -> Double
toDegrees r = r * 180 / pi

fromDegrees :: Double -> Double
fromDegrees d = d * pi / 180

data Bomb = Bomb Rectangular Double Double Double  -- (x, y), a, b, r

parseBomb :: String -> Bomb
parseBomb str = case map read (words str) of
    [x, y, a, b, r] -> Bomb (x, y) (fromDegrees a) (fromDegrees b) r
    _               -> error "parseBomb"

bombLines :: Bomb -> [Line]
bombLines (Bomb (x, y) a b r) =
    [((x, y), end1), ((x, y), end2)] -- , (end1, end2)]
  where
    end1 = let (dx, dy) = fromPolar (r, b + a / 2) in (x + dx, y + dy)
    end2 = let (dx, dy) = fromPolar (r, b - a / 2) in (x + dx, y + dy)

inRadius :: Bomb -> Rectangular -> Bool
inRadius (Bomb (bx, by) ba bb br) (x, y)
    | r > br    = False
    | otherwise = q >= bb - ba / 2 && q <= bb + ba / 2
  where
    (r, q) = toPolar (x - bx, y - by)

lineIntersection :: Line -> Line -> Bool
lineIntersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
    | d == 0                                     = False
    | x < min x1 x2 - eps || x > max x1 x2 + eps = False
    | x < min x3 x4 - eps || x > max x3 x4 + eps = False
    | y < min y1 y2 - eps || y > max y1 y2 + eps = False
    | y < min y3 y4 - eps || y > max y3 y4 + eps = False
    | otherwise                                  = True
  where
    d    = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    pre  = x1 * y2 - y1 * x2
    post = x3 * y4 - y3 * x4
    x    = (pre * (x3 - x4) - (x1 - x2) * post) / d
    y    = (pre * (y3 - y4) - (y1 - y2) * post) / d

    -- Gotta love floating points
    eps = 0.0000001

data Soldier = Soldier Rectangular Double Double  -- (x, y) d h

parseSoldier :: String -> Soldier
parseSoldier str = case map read (words str) of
    [x, y, d, h] -> Soldier (x, y) d h
    _            -> error "parseSoldier"

soldierPoints :: Soldier -> [Rectangular]
soldierPoints (Soldier (x, y) d h) =
    [ (x + dx, y + dy)
    | i <- [0 .. h - 1]
    , let (dx, dy) = fromPolar (d / 2, i * step)
    ]
  where
    step = 2 * pi / h

soldierLines :: Soldier -> [Line]
soldierLines soldier = zip points points'
  where
    points  = soldierPoints soldier
    points' = tail points ++ [head points]

kills :: [Bomb] -> Soldier -> Bool
kills bombs soldier =
    killsByEdge bombs soldier ||
    (any (\b -> killsBySplitting b soldier) bombs)

killsByEdge :: [Bomb] -> Soldier -> Bool
killsByEdge bombs soldier =
    any (== (True, True)) $
    map snd $
    foldl step (zip (soldierLines soldier) $ repeat (False, False)) bombs
  where
    step ls bomb                    = map (flip step' bomb) ls
    step' ((p1, p2), (h1, h2)) bomb =
        ((p1, p2), (h1 || inRadius bomb p1, h2 || inRadius bomb p2))
      
killsBySplitting :: Bomb -> Soldier -> Bool
killsBySplitting bomb soldier =
    sum [length (splits l) | l <- bombLines bomb] > 2
  where
    splits l = filter (lineIntersection l) (soldierLines soldier)

main :: IO ()
main = do
    cases <- readLn
    replicateM_ cases $ do
        [numBombs, numSoldiers] <- map read . words <$> getLine
        bombs    <- map parseBomb    <$> replicateM numBombs getLine
        soldiers <- map parseSoldier <$> replicateM numSoldiers getLine
        forM_ soldiers $ \soldier -> putStrLn $
            if kills bombs soldier then "dood" else "levend"
