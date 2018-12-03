
module Main (main) where

import Common
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Text.Megaparsec.Char (char, string)

main :: IO ()
main = do
    areas <- readParsedLines rectP 3

    let claims = mkClaims areas

    print (part1 claims)
    print (part2 claims areas)
    pure ()

part1 :: Claims -> Int
part1 = length . M.filter (> 1)

part2 :: Claims -> [Area] -> Int
part2 fabric = areaId . fromJust . find (all (\p -> fabric M.! p == 1) . areaPoints)

mkClaims :: [Area] -> Claims
mkClaims = foldr (flip insertArea) M.empty
    where
    insertArea :: Claims -> Area -> Claims
    insertArea m = foldr (\point -> M.insertWith (+) point 1) m . areaPoints

type Claims = M.Map (Int,Int) Int

data Area = Area { areaId  :: Int
                 , areaPoints :: [(Int,Int)]
                 } deriving Show

rectP :: Parser Area
rectP = do
    _              <- char '#'
    rectNum        <- intP
    _              <- string " @ "
    (minX,minY)    <- (,) <$> intP <* char ',' <*> intP
    _              <- string ": "
    (width,height) <- (,) <$> intP <* char 'x' <*> intP

    pure (Area rectNum [(x,y) | x <- [minX .. minX + width  - 1]
                              , y <- [minY .. minY + height - 1]])
