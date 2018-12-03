
module Main (main) where

import Common
import Data.List (group, sort, tails)

main :: IO ()
main = do
    input <- lines <$> readInput 2 :: IO [String]
    print (part1 input)
    print (part2 input)

part1 :: [String] -> Int
part1 xs = length twos * length threes
  where
  sizes = map (map length . group . sort) xs
  twos = filter (elem 2) sizes
  threes = filter (elem 3) sizes

part2 :: [String] -> String
part2 xs = head [concat filtered
                | (this:ys) <- tails xs
                , that      <- ys
                , hamming this that == 1
                , let filtered = zipWith (\x y -> if x == y then [x] else []) this that
                ]

hamming :: Eq a => [a] -> [a] -> Int
hamming xs = sum . zipWith (\x y -> if x == y then 0 else 1) xs
