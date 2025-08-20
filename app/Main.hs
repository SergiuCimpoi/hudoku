module Main (main) where

import Base (solve)
import Data (s439)
import Data.Time (diffUTCTime, getCurrentTime)
import Infer (MarkingBoard (..), colToCells, digitsToCellIds, mark, reduceSingles, rowToCells)

backtracking :: IO ()
backtracking = do
    startTime <- getCurrentTime
    let solutions = solve s439
    mapM_ print solutions
    endTime <- getCurrentTime
    putStrLn $ "Execution time: " ++ show (diffUTCTime endTime startTime)

inference :: IO ()
inference = do
    startTime <- getCurrentTime
    let result = reduceSingles $ mark s439
    print result
    endTime <- getCurrentTime
    putStrLn $ "Execution time: " ++ show (diffUTCTime endTime startTime)
    print $ rowToCells result 1
    print $ digitsToCellIds (rowToCells result 1)
    print $ digitsToCellIds (colToCells result 1)

main :: IO ()
main = inference
