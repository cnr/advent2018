
module Main (main) where

import Common
import Text.Megaparsec.Char (string)

main :: IO ()
main = do
    towers <- readParsedLines towerP 6
    print (part1 towers)
    print (part2 towers)


---- Part 1

part1 :: [Tower] -> Int
part1 = maximum . map (uncurry towerArea) . filter (uncurry finite) . pick

towerArea :: Tower -> [Tower] -> Int
towerArea t ts = length $ dfs (filter (closest t ts) . nesw) t

-- Check if a point is closest to our tower (True) or a different tower (False)
closest :: Tower -> [Tower] -> Point -> Bool
closest t ts point = dist t < minimum (map dist ts)
    where
    dist = manhattan point

-- Is a tower's area finite?
finite :: Tower -> [Tower] -> Bool
finite t = and . sequence (finiteCriteria t)

finiteCriteria :: Tower -> [[Tower] -> Bool]
finiteCriteria t1 =
    [ any (\t2 -> fst t2 - fst t1 >= manhattan t1 t2 `div` 2) -- X+
    , any (\t2 -> snd t2 - snd t1 >= manhattan t1 t2 `div` 2) -- Y+
    , any (\t2 -> fst t1 - fst t2 >= manhattan t1 t2 `div` 2) -- X-
    , any (\t2 -> snd t1 - snd t2 >= manhattan t1 t2 `div` 2) -- Y-
    ]


---- Part 2

part2 :: [Tower] -> Int
part2 towers = length $
    dfs (\point -> [point' | point' <- nesw point
                           , sum (map (manhattan point') towers) < 10000]) center
    where
    center = (avg (map fst towers), avg (map snd towers))
    avg = div <$> sum <*> length


---- Shared

nesw :: Point -> [Point]
nesw (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

manhattan :: Point -> Point -> Int
manhattan (x,y) (x',y') = abs (x - x') + abs (y - y')

type Tower = (Int,Int)
type Point = (Int,Int)

towerP :: Parser Tower
towerP = (,) <$> intP <* string ", " <*> intP
