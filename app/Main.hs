module Main (main) where

import Base (solve)
import Data (s439)
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import Infer (MarkingBoard (..), MarkingCell (..), blockToCells, colToCells, digitsToCellIds, mark, reduceHiddenPairs', reduceSingles, rowToCells, rowToCellsV)

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

    let marking = mark s439
    print marking
    -- print $ rowToCells marking 1
    print $ digitsToCellIds (rowToCells marking 0)
    -- print $ digitsToCellIds (colToCells marking 1)
    -- print $ digitsToCellIds (blockToCells marking 0 0)

    let x = reduceHiddenPairs' (rowToCellsV marking 0)
    -- let x =
    --         reduceHiddenPairs' $
    --             V.fromList
    --                 [ Candidates [3, 8]
    --                 , Value 2
    --                 , Candidates [3, 6]
    --                 , Candidates [3, 4, 6]
    --                 , Value 5
    --                 , Candidates [3, 4]
    --                 , Candidates [1, 7, 8]
    --                 , Value 9
    --                 , Candidates [1, 6, 7]
    --                 ]
    print x

main :: IO ()
main = inference
