module Main (main) where

import Base (solve)
import Data (s439)
import Data.Time (diffUTCTime, getCurrentTime)

main :: IO ()
main = do
    startTime <- getCurrentTime
    let solutions = solve s439
    mapM_ print solutions
    endTime <- getCurrentTime
    putStrLn $ "Execution time: " ++ show (diffUTCTime endTime startTime)
