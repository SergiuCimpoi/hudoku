module Lib (Board (..), findNextEmpty, update, solve, possible, checkBoard) where

import Data.Foldable (minimumBy)
import Data.List (findIndex)
import Data.Maybe (catMaybes, isNothing)

type Cell = Maybe Int
newtype Board = Board [[Cell]] deriving (Eq)

instance Show Board where
    show (Board rows) = unlines (map showRow rows)
      where
        showRow row = unwords (map showCell row)
        showCell Nothing = "."
        showCell (Just n) = show n

hLine :: Board -> Int -> [Cell]
hLine (Board rows) h = rows !! h

vLine :: Board -> Int -> [Cell]
vLine (Board rows) v = (!! v) <$> rows

block :: Board -> Int -> Int -> [Cell]
block (Board rows) i j = concatMap (take 3 . drop (3 * j)) blockRows
  where
    blockRows = take 3 (drop (3 * i) rows)

hasDuplicates :: [Int] -> Bool
hasDuplicates [] = False
hasDuplicates (n : ns) = elem n ns || hasDuplicates ns

isValid :: [Cell] -> Bool
isValid = not . hasDuplicates . catMaybes

checkBoard :: Board -> Bool
checkBoard board = checkHLines && checkVLines && checkBlocks
  where
    checkHLines = all (isValid . hLine board) [0 .. 8]
    checkVLines = all (isValid . vLine board) [0 .. 8]
    checkBlocks =
        all
            (\(r, c) -> isValid (block board r c))
            [(r, c) | r <- [0 .. 2], c <- [0 .. 2]]

-- TODO: scans the row containing Nothing twice
-- can be optimised by explicit recursion over all rows
findNextEmpty :: Board -> Maybe (Int, Int)
findNextEmpty (Board rows) = do
    row <- findIndex (any isNothing) rows
    col <- findIndex isNothing (rows !! row)
    return (row, col)

-- find the empty position (row, col) with least possible numbers
findBestEmpty :: Board -> Maybe ((Int, Int), [Int])
findBestEmpty board@(Board rows) =
    let allEmptyPos = [((r, c), possible board (r, c)) | r <- [0 .. 8], c <- [0 .. 8], isNothing $ rows !! r !! c]
     in if null allEmptyPos
            then Nothing
            else Just $ minimumBy (\a b -> (length . snd) a `compare` (length . snd) b) allEmptyPos

update :: Board -> (Int, Int) -> Maybe Int -> Board
update (Board rows) (r, c) val =
    case splitAt r rows of
        (top, oldRow : bottom) -> case splitAt c oldRow of
            (left, _ : right) ->
                let newRow = left ++ [val] ++ right
                 in Board $ top ++ [newRow] ++ bottom
            (_, []) -> Board rows
        (_, []) -> Board rows

possible :: Board -> (Int, Int) -> [Int]
possible board (r, c) =
    [ n
    | n <- [1 .. 9]
    , n `notElem` catMaybes (hLine board r)
    , n `notElem` catMaybes (vLine board c)
    , n `notElem` catMaybes (block board (r `div` 3) (c `div` 3))
    ]

solve :: Board -> [Board]
solve board =
    case findBestEmpty board of
        Nothing -> [board]
        Just (pos, ns) ->
            let candidates = update board pos . Just <$> ns
             in concatMap solve candidates
