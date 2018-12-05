
module Main (main) where

import Common
import Control.Applicative ((<|>))
import Data.Foldable (maximumBy)
import Data.List (group, sort)
import Data.Ord (comparing)
import Text.Megaparsec.Char (char, string)

main :: IO ()
main = do
    logs <- readParsedLines logEntryP 4
    let guardSleep = compile logs

    print (part1 guardSleep)
    print (part2 guardSleep)

part1 :: GuardSleep -> Int
part1 sleep = let (guard ,_) = maximumBy (comparing snd) (card (map fst sleep))
                  (minute,_) = maximumBy (comparing snd) (card (map snd (filter ((== guard) . fst) sleep)))
               in guard * minute

part2 :: GuardSleep -> Int
part2 sleep = let ((guard,minute),_) = maximumBy (comparing snd) (card sleep)
               in guard * minute

card :: Ord a => [a] -> [(a,Int)]
card = map (\xs -> (head xs, length xs)) . group . sort

type GuardSleep = [(Int,Int)] {- guard, minute -}

compile :: [LogEntry] -> GuardSleep
compile = go (error "Expecting StartShift") . sort
    where
    go :: Int -> [LogEntry] -> GuardSleep
    go _ [] = []
    go g (e:e':es) = case (entryAction e, entryAction e') of
        (StartShift g', _     ) -> go g' (e':es)
        (FallAsleep   , WakeUp) -> [(g,t) | t <- [timeMinute (entryTime e) .. timeMinute (entryTime e') - 1]] ++ go g es
        _ -> error "Invalid log entries"
    go _ _ = error "Invalid log entries"

logEntryP :: Parser LogEntry
logEntryP = LogEntry <$  char '['
                     <*> dateP
                     <*  char ' '
                     <*> timeP
                     <*  string "] "
                     <*> actionP

dateP :: Parser Date
dateP = Date <$> intP <* char '-' <*> intP <* char '-' <*> intP

timeP :: Parser Time
timeP = Time <$> intP <* char ':' <*> intP

actionP :: Parser Action
actionP = StartShift <$ string "Guard #" <*> intP <* string " begins shift"
      <|> FallAsleep <$ string "falls asleep"
      <|> WakeUp     <$ string "wakes up"

data LogEntry = LogEntry { entryDate   :: Date
                         , entryTime   :: Time
                         , entryAction :: Action
                         } deriving (Eq, Ord, Show)

data Date = Date { dateYear :: Int, dateMonth :: Int, dateDay :: Int } deriving (Eq, Ord, Show)
data Time = Time { timeHour :: Int, timeMinute :: Int } deriving (Eq, Ord, Show)
data Action = StartShift Int | FallAsleep | WakeUp deriving (Eq, Ord, Show)
