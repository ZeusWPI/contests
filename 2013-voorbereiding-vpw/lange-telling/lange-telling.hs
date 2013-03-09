--------------------------------------------------------------------------------
import           Control.Monad (replicateM_)
import           Data.List     (intercalate)
import           Data.Time     (Day, diffDays, fromGregorian)


--------------------------------------------------------------------------------
newtype DaysSinceEpoch = DaysSinceEpoch Integer
    deriving (Show)


--------------------------------------------------------------------------------
parseGregorian :: String -> Day
parseGregorian str = fromGregorian (read year) (read month) (read day)
  where
    [day, month, year] = split str
    split              = words . map (\c -> if c == '/' then ' ' else c)


--------------------------------------------------------------------------------
epoch :: Day
epoch = fromGregorian 1970 1 1


--------------------------------------------------------------------------------
fromDay :: Day -> DaysSinceEpoch
fromDay t1 = DaysSinceEpoch $ t1 `diffDays` epoch


--------------------------------------------------------------------------------
newtype MayanDay = MayanDay Integer
    deriving (Show)


--------------------------------------------------------------------------------
toMayanDay :: DaysSinceEpoch -> MayanDay
toMayanDay (DaysSinceEpoch d1) =
    let MayanDay d2 = mayanEpoch in MayanDay (d1 + d2)


--------------------------------------------------------------------------------
-- | 12.17.16.7.5
mayanEpoch :: MayanDay
mayanEpoch = MayanDay $
    5 + 20 * (7 + 18 * (16 + 20 * (17 + 20 * 12)))


--------------------------------------------------------------------------------
formatMayanDay :: MayanDay -> String
formatMayanDay (MayanDay n) = intercalate "." $ map show
    [baktuns, katuns, tuns, uinals, kin]
  where
    (baktuns, n1) = n  `divMod` (20 * 20 * 18 * 20)
    (katuns,  n2) = n1 `divMod` (20 * 18 * 20)
    (tuns,    n3) = n2 `divMod` (18 * 20)
    (uinals,  n4) = n3 `divMod` 20
    kin           = n4


--------------------------------------------------------------------------------
gregorianToMayan :: String -> String
gregorianToMayan = formatMayanDay . toMayanDay . fromDay . parseGregorian


--------------------------------------------------------------------------------
main :: IO ()
main = do
    testCases <- readLn
    replicateM_ testCases $ getLine >>= putStrLn . gregorianToMayan
