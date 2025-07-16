module Debug (solveDbg)
where

import Debug.Trace (traceShow)

import Lib (Board (..), checkBoard, findNextEmpty, solve, update)

getCellAt :: (Int, Int) -> Board -> Maybe Int
getCellAt (row, col) (Board rows) = (rows !! row) !! col

solveDbg :: Board -> [Board]
solveDbg board = case findNextEmpty board of
    Nothing -> [board]
    Just pos ->
        let candidates = filter checkBoard $ update board pos . Just <$> [1 .. 9]
         in traceShow ("Candidates at", pos, map (getCellAt pos) candidates) $
                concatMap solve candidates
