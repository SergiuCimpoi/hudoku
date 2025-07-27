module Debug (solveDbg)
where

import Debug.Trace (traceShow)

import Lib (Board (..), findBestEmpty, solve, update)

getCellAt :: (Int, Int) -> Board -> Maybe Int
getCellAt (row, col) (Board rows) = (rows !! row) !! col

solveDbg :: Board -> [Board]
solveDbg board =
    case findBestEmpty board of
        Nothing -> [board]
        Just (pos, ns) ->
            let candidates = update board pos . Just <$> ns
             in traceShow ("Candidates at", pos, map (getCellAt pos) candidates) $
                    concatMap solve candidates

-- sum1 :: [Int] -> Int
-- sum1 (x : xs) = x + sum1 xs
-- sum1 [] = 0
--
-- sum2 :: [Int] -> Int
-- sum2 (x : xs) = sum2 xs + x
-- sum2 [] = 0
--
-- sum3 :: [Int] -> Int
-- sum3 = sum_ 0
--   where
--     sum_ acc (x : xs) = sum_ (acc + x) xs
--     sum_ acc [] = acc
--
-- main :: IO ()
-- main = do
--     startTime <- getCurrentTime
--     let n = sum3 [1 .. 10000000]
--     print n
--     -- let solutions = solve s439
--     -- mapM_ print solutions
--     endTime <- getCurrentTime
--     putStrLn $ "Execution time: " ++ show (diffUTCTime endTime startTime)
