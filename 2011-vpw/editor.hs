-- Solution to http://www.vlaamseprogrammeerwedstrijd.be/2011/opgaves/cat1-2011/editor.pdf
import Control.Monad
import Control.Applicative

import Data.List

data Command = Command
        { times :: Int
        , word  :: String
        } deriving (Show)

parseCommand :: String -> Command
parseCommand str = Command times word
  where
    [times', word] = words str
    times = read times'


remove ::  [String] -> Command -> [String]
remove words (Command times word) = concat $ map reduce $ group words
  where
    reduce w@(w1:_)
        | w1 /= word = w
        | otherwise  = let (l,r) = length w `divMod` times
                       in replicate l w1 ++ replicate r w1

removeAll :: [Command] -> String -> [String]
removeAll cmds line = foldl remove (words line) cmds

main = do
    cases <- readLn
    replicateM_ cases $ do
        lines <- readLn
        text  <- replicateM lines getLine

        numcmds  <- readLn
        commands <- replicateM numcmds (parseCommand <$> getLine)

        mapM putStrLn $ fmap (unwords . removeAll commands) $ text


