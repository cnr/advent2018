
{-# language RecordWildCards #-}

module Main (main) where

import Common
import Data.Char (ord)
import qualified Control.Monad.State as S
import Data.List (delete, partition, sort)
import qualified Data.Map.Strict as M
import Text.Megaparsec.Char (letterChar, string)

main :: IO ()
main = do
    reqs <- readParsedLines reqP 7
    let nodes = buildNodes reqs

    print (part1 nodes)
    print (part2 nodes)


---- Part 1

part1 :: Nodes -> String
part1 nodes = case sort (available nodes) of
    []       -> []
    (node:_) -> node : (part1 $ complete node nodes)


---- Part 2

part2 :: Nodes -> Int
part2 nodes = timeElapsed $ S.execState simulate (WorkState nodes [] 0)

createTask :: Char -> Task
createTask c = Task c duration
    where
    duration = ord c - ord 'A' + 61

numWorkers :: Int
numWorkers = 5

simulate :: WorkS ()
simulate = do
    nodes <- S.gets remainingWork

    if M.null nodes
        then pure ()
        else step *> simulate

step :: WorkS ()
step = assignWorkers *> fastForward
    
assignWorkers :: WorkS ()
assignWorkers = do
    nodes         <- S.gets remainingWork
    activeWorkers <- length <$> S.gets activeWork

    let waitingWorkers = numWorkers - activeWorkers
        tasks = map createTask (take waitingWorkers (available nodes))
        newRemainingWork = foldr M.delete nodes (available nodes)

    S.modify (\s -> s { remainingWork = newRemainingWork
                      , activeWork = tasks ++ activeWork s })

fastForward :: WorkS ()
fastForward = do
    WorkState{..} <- S.get

    let timeSkip       = minimum [time | Task _ time <- activeWork]
        (done,pending) = partition ((== timeSkip) . taskTime) activeWork

        newRemainingWork = foldr (complete . taskName) remainingWork done
        newActiveWork    = map (\task -> task { taskTime = taskTime task - timeSkip }) pending
        newTimeElapsed   = timeSkip + timeElapsed

    S.put (WorkState newRemainingWork newActiveWork newTimeElapsed)

type WorkS = S.State WorkState

data WorkState = WorkState { remainingWork :: Nodes
                           , activeWork    :: [Task]
                           , timeElapsed   :: Int
                           } deriving (Eq, Ord, Show)

data Task = Task { taskName :: Char
                 , taskTime :: Int {- time remaining -}
                 } deriving (Eq, Ord, Show)



---- Shared

available :: Nodes -> [Char]
available nodes = sort [n | (n,[]) <- M.assocs nodes]

complete :: Char -> Nodes -> Nodes
complete node nodes = delete node <$> M.delete node nodes

type Req = (Char,Char) -- X is a prerequisite for Y

type Nodes = M.Map Char [Char] -- X has [prerequisites]

buildNodes :: [Req] -> Nodes
buildNodes = foldr (\(pre,x) -> M.insertWith (const id) pre [] . M.insertWith (++) x [pre]) M.empty

reqP :: Parser Req
reqP = (,) <$ string "Step " <*> letterChar <* string " must be finished before step " <*> letterChar <* string " can begin."
