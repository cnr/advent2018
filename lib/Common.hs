
module Common
    ( Parser
    , intP
    , readInput
    , readParsed
    , readParsedLines
    
    , dfs
    ) where

import qualified Data.Set as S
import           Data.Void (Void)
import           Text.Megaparsec ((<|>), Parsec, parse)
import           Text.Megaparsec.Char (char)
import           Text.Megaparsec.Char.Lexer (decimal)

-- Parsing

type Parser = Parsec Void String

intP :: Parser Int
intP = negate <$ char '-' <*> decimal
   <|>                        decimal
readInput :: Int -> IO String
readInput n = readFile $ "input/day" ++ show n ++ ".txt"

readParsed :: Parser a -> Int -> IO a
readParsed parser = fmap (unsafeParse parser) . readInput

readParsedLines :: Parser a -> Int -> IO [a]
readParsedLines parser n = map (unsafeParse parser) . lines <$> readInput n

unsafeParse :: Parser a -> String -> a
unsafeParse parser input = result where Right result = parse parser "[source]" input

-- Graph traversal

dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs f seed = go S.empty [seed]
    where
    go _    [] = []
    go seen (x:xs)
      | S.member x seen = go seen xs
      | otherwise = x : go (S.insert x seen) (f x ++ xs)
