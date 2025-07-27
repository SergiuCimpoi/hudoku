module Lib_1 (Board (..), hLine, vLine, block, s439, valid) where

import Data.Foldable (minimumBy)
import qualified Data.Matrix as M
import Data.Maybe (catMaybes, isNothing)
import qualified Data.Vector as V

type Cell = Maybe Int
newtype Board = Board (M.Matrix Cell) deriving (Eq)

instance Show Board where
    show (Board mat) = unlines (fmap showRow (M.toLists mat))
      where
        showRow row = unwords (fmap showCell row)
        showCell Nothing = "."
        showCell (Just n) = show n

hLine :: Board -> Int -> V.Vector Cell
hLine (Board mat) r = M.getRow r mat

--
vLine :: Board -> Int -> V.Vector Cell
vLine (Board mat) c = M.getCol c mat

block :: Board -> Int -> Int -> M.Matrix Cell
block (Board mat) i j = M.submatrix startRow (startRow + 2) startCol (startCol + 2) mat
  where
    startRow = 3 * (i - 1) + 1
    startCol = 3 * (j - 1) + 1

s439 :: Board
s439 =
    Board $
        M.fromLists
            [ [Nothing, Nothing, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Just 7, Just 9, Just 2, Nothing, Nothing, Nothing, Just 8, Just 1]
            , [Nothing, Nothing, Just 5, Nothing, Just 1, Nothing, Just 2, Nothing, Nothing]
            , [Just 9, Just 6, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Just 2, Nothing, Just 7, Nothing, Nothing, Nothing, Just 8, Nothing, Just 9]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 2, Just 6]
            , [Nothing, Nothing, Just 8, Nothing, Just 2, Nothing, Just 5, Nothing, Nothing]
            , [Just 1, Just 2, Nothing, Nothing, Nothing, Just 3, Just 7, Just 4, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Just 4, Just 6, Nothing, Nothing, Nothing]
            ]

valid :: Board
valid =
    Board $
        M.fromLists
            [ [Just 5, Just 3, Just 4, Just 6, Just 7, Just 8, Just 9, Just 1, Just 2]
            , [Just 6, Just 7, Just 2, Just 1, Just 9, Just 5, Just 3, Just 4, Just 8]
            , [Just 1, Just 9, Just 8, Just 3, Just 4, Just 2, Just 5, Just 6, Just 7]
            , ------------------------------------------------------------------------
              [Just 8, Just 5, Just 9, Just 7, Just 6, Just 1, Just 4, Just 2, Just 3]
            , [Just 4, Just 2, Just 6, Just 8, Just 5, Just 3, Just 7, Just 9, Just 1]
            , [Just 7, Just 1, Just 3, Just 9, Just 2, Just 4, Just 8, Just 5, Just 6]
            , ------------------------------------------------------------------------
              [Just 9, Just 6, Just 1, Just 5, Just 3, Just 7, Just 2, Just 8, Just 4]
            , [Just 2, Just 8, Just 7, Just 4, Just 1, Just 9, Just 6, Just 3, Just 5]
            , [Just 3, Just 4, Just 5, Just 2, Just 8, Just 6, Just 1, Just 7, Just 9]
            ]

-- hasDuplicates :: [Int] -> Bool
-- hasDuplicates [] = False
-- hasDuplicates (n : ns) = elem n ns || hasDuplicates ns
--
-- isValid :: [Cell] -> Bool
-- isValid = not . hasDuplicates . catMaybes
--
-- checkBoard :: Board -> Bool
-- checkBoard board = checkHLines && checkVLines && checkBlocks
--   where
--     checkHLines = all (isValid . hLine board) [0 .. 8]
--     checkVLines = all (isValid . vLine board) [0 .. 8]
--     checkBlocks =
--         all
--             (\(r, c) -> isValid (block board r c))
--             [(r, c) | r <- [0 .. 2], c <- [0 .. 2]]
--
-- update :: Board -> (Int, Int) -> Maybe Int -> Board
-- update (Board rows) (r, c) val =
--     case splitAt r rows of
--         (top, oldRow : bottom) -> case splitAt c oldRow of
--             (left, _ : right) ->
--                 let newRow = left ++ [val] ++ right
--                  in Board $ top ++ [newRow] ++ bottom
--             (_, []) -> Board rows
--         (_, []) -> Board rows
--
-- possible :: Board -> (Int, Int) -> [Int]
-- possible board (r, c) =
--     [ n
--     | n <- [1 .. 9]
--     , n `notElem` catMaybes (hLine board r)
--     , n `notElem` catMaybes (vLine board c)
--     , n `notElem` catMaybes (block board (r `div` 3) (c `div` 3))
--     ]
--
-- -- find the empty position (row, col) with least possible numbers
-- findBestEmpty :: Board -> Maybe ((Int, Int), [Int])
-- findBestEmpty board@(Board rows) =
--     let allEmptyPos = [((r, c), possible board (r, c)) | r <- [0 .. 8], c <- [0 .. 8], isNothing $ rows !! r !! c]
--      in if null allEmptyPos
--             then Nothing
--             else Just $ minimumBy (\a b -> (length . snd) a `compare` (length . snd) b) allEmptyPos
--
-- solve :: Board -> [Board]
-- solve board =
--     case findBestEmpty board of
--         Nothing -> [board]
--         Just (pos, ns) ->
--             let boards = update board pos . Just <$> ns
--              in concatMap solve boards
