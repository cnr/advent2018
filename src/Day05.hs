
module Main (main) where

import Common
import Data.Char (toLower)

main :: IO ()
main = do
    input <- readInput 5
    print (part1 input)
    print (part2 input)

part1 :: String -> Int
part1 = length . reduce

part2 :: String -> Int
part2 = minimum . map (length . reduce) . removingElems
    where
    removingElems :: String -> [String]
    removingElems xs = [filter (\x -> x /= c && x /= toLower c) xs | c <- ['A'..'Z']]

reduce :: String -> String
reduce = foldr combine ""

combine :: (Char -> String -> String)
combine c []     = [c]
combine c (x:xs)
  | merges c x = xs
  | otherwise  = (c:x:xs)

merges :: Char -> Char -> Bool
merges x y = x /= y && toLower x == toLower y
