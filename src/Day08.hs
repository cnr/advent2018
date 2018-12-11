
module Main (main) where

import Common
import Control.Applicative (many)
import Control.Monad (replicateM)
import Data.Tree (foldTree, Tree(..))
import Text.Megaparsec.Char (spaceChar)

main :: IO ()
main = do
    tree <- readParsed treeP 8
    print (part1 tree)
    print (part2 tree)

part1 :: Tree [Int] -> Int
part1 = foldTree (\xs ys -> sum (xs ++ ys))

part2 :: Tree [Int] -> Int
part2 (Node meta []) = sum meta
part2 (Node meta xs) = sum [part2 child | Just child <- map ((xs !?) . subtract 1) meta]

treeP :: Parser (Tree [Int])
treeP = do
    numChildren <- next
    numMeta     <- next

    flip Node <$> replicateM numChildren treeP
              <*> replicateM numMeta next

next :: Parser Int
next = intP <* many spaceChar
