
module Main (main) where

import Common
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Text.Megaparsec.Char (string)

main :: IO ()
main = do
    (players,marbles) <- readParsed inputP 9
    print (playGame players marbles)
    print (playGame players (marbles * 100))

playGame :: Int -> Int -> Int
playGame playerCount marbleCount = go M.empty 1 (S.singleton 0)
    where
    go :: M.Map Int Int -> Int -> S.Seq Int -> Int
    go scores m board
        | m > marbleCount = maximum scores
        | m `mod` 23 == 0 = case rotate (-7) board of
            x S.:<| xs -> go (M.insertWith (+) (m `mod` playerCount) (m+x) scores) (m+1) xs
            _          -> error "empty board"
        | otherwise       = go scores (m+1) (m S.<| rotate 2 board)

rotate :: Int -> S.Seq a -> S.Seq a
rotate n xs = right <> left
    where (left,right) = S.splitAt (n `mod` S.length xs) xs

inputP :: Parser (Int,Int)
inputP = (,) <$> intP <* string " players; last marble is worth " <*> intP <* string " points"
