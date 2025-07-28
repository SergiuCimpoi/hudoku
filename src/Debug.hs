module Debug (solveDbg)
where

import Debug.Trace (traceShow)

import Base (Board (..), findBestEmpty, solve, update)
import qualified Data.Matrix as M

solveDbg :: Board -> [Board]
solveDbg board =
    case findBestEmpty board of
        Nothing -> [board]
        Just (pos, ns) ->
            let candidates = update board pos . Just <$> ns
             in traceShow ("Candidates at", pos, map ((M.! pos) . getMatrix) candidates) $
                    concatMap solve candidates

-- solve :: Board -> [Board]
-- solve board =
--     case findBestEmpty board of
--         Nothing -> [board]
--         Just (pos, ns) ->
--             let boards = update board pos . Just <$> ns
--              in concatMap solve boards

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
