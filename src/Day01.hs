
module Main (main) where

import Common
import qualified Data.Set as S

main :: IO ()
main = do
    input <- map read . lines . filter (/= '+') <$> readInput 1 :: IO [Int]
    print $ sum input
    print $ part2 input

part2 :: [Int] -> Int
part2 = go S.empty . scanl (+) 0 . cycle
  where
  go seen (x:xs)
    | S.member x seen = x
    | otherwise       = go (S.insert x seen) xs
  go _ _ = error "impossible"
