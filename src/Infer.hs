module Infer (mark, reduceSingles) where

import Base (Board (..))
import qualified Data.Matrix as M
import Data.Maybe (fromJust, isJust)

-- inferences

data MarkingCell = Value Int | Candidates [Int] deriving (Eq)

instance Show MarkingCell where
    show (Value v) = 'v' : show v
    show (Candidates []) = ""
    show (Candidates (c : cs)) = show c ++ "," ++ show (Candidates cs)

newtype MarkingBoard = MarkingBoard (M.Matrix MarkingCell) deriving (Eq, Show)

isSingle :: MarkingCell -> Bool
isSingle (Candidates (l : [])) = True
isSingle _ = False

getSingle :: MarkingCell -> Int
getSingle (Candidates (l : [])) = l
getSingle _ = undefined

initialMarkedBoard :: MarkingBoard
initialMarkedBoard = MarkingBoard $ M.matrix 9 9 (const $ Candidates [1 .. 9])

blockFromPos :: (Int, Int) -> (Int, Int)
blockFromPos (r, c) = ((r - 1) `div` 3, (c - 1) `div` 3)

isPosInBlock :: (Int, Int) -> (Int, Int) -> Bool
isPosInBlock (r, c) (i, j) = r >= startRow && r < startRow + 3 && c >= startCol && c < startCol + 3
  where
    startRow = 3 * i + 1
    startCol = 3 * j + 1

addValue :: MarkingBoard -> (Int, Int) -> Int -> MarkingBoard
addValue (MarkingBoard mat) (r, c) n =
    MarkingBoard $
        M.matrix
            9
            9
            ( \(i, j) ->
                if i == r && j == c
                    then
                        Value n
                    else case mat M.! (i, j) of
                        Value v -> Value v
                        Candidates cs ->
                            Candidates $
                                if i == r || j == c || isPosInBlock (i, j) (blockFromPos (r, c))
                                    then filter (/= n) cs
                                    else cs
            )

mark :: Board -> MarkingBoard
mark (Board mat) =
    let elems = [(r, c, fromJust (mat M.! (r, c))) | c <- [1 .. 9], r <- [1 .. 9], isJust $ mat M.! (r, c)]
     in foldr (\(r, c, n) b -> addValue b (r, c) n) initialMarkedBoard elems

reduceSingles :: MarkingBoard -> MarkingBoard
reduceSingles board@(MarkingBoard mat) =
    let elems = [(r, c, getSingle (mat M.! (r, c))) | c <- [1 .. 9], r <- [1 .. 9], isSingle $ mat M.! (r, c)]
     in foldr (\(r, c, n) b -> addValue b (r, c) n) board elems
