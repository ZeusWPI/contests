

type Rectangular = (Double, Double)

type Line = (Rectangular, Rectangular)

type Polar = (Double, Double)

toPolar :: Rectangular -> Polar
toPolar (x, y) = (sqrt (x * x + y * y), atan (y / x))

fromPolar :: Polar -> Rectangular
fromPolar (r, q) = (r * cos q, r * sin q)

data Bomb = Bomb Rectangular Double Double Double  -- (x, y), a, b, r

inRadius :: Bomb -> Rectangular -> Bool
inRadius (Bomb (bx, by) ba bb br) (x, y)
    | r > br    = False
    | otherwise = q >= ba && q < ba + bb
  where
    (r, q) = toPolar (x - bx, y - by)

lineIntersection :: Line -> Line -> Bool
lineIntersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
    | d == 0                       = False
    | x < minimum [x1, x2, x3, x4] = False
    | x > maximum [x1, x2, x3, x4] = False
    | y < minimum [y1, y2, y3, y4] = False
    | y > maximum [y1, y2, y3, y4] = False
    | otherwise                    = True
  where
    d    = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    pre  = x1 * y2 - y1 * x2
    post = x3 * y4 - y3 * x4
    x    = (pre * (x3 - x4) - (x1 - x2) * post) / d
    y    = (pre * (y3 - y4) - (y1 - y2) * post) / d
