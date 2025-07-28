module Infer () where

import Base (Board (..))
import qualified Data.Matrix as M

-- inferences

data Cell = Value Int | Candidates [Int] deriving (Eq)

instance Show Cell where
    show (Value v) = 'v' : show v
    show (Candidates []) = ""
    show (Candidates (c : cs)) = show c ++ "," ++ show (Candidates cs)

newtype MarkedBoard = MarkedBoard (M.Matrix Cell) deriving (Eq, Show)

candidates :: Board -> (Int, Int) -> Cell
candidates b (r, c) = undefined

initialMarkedBoard :: MarkedBoard
initialMarkedBoard = MarkedBoard $ M.matrix 9 9 (const $ Candidates [1 .. 9])

blockFromPos :: (Int, Int) -> (Int, Int)
blockFromPos (r, c) = ((r - 1) `div` 3, (c - 1) `div` 3)

isPosInBlock :: (Int, Int) -> (Int, Int) -> Bool
isPosInBlock (r, c) (i, j) = r >= startRow && r < startRow + 3 && c >= startCol && c < startCol + 3
  where
    startRow = 3 * i + 1
    startCol = 3 * j + 1

addValue :: MarkedBoard -> (Int, Int) -> Int -> MarkedBoard
addValue (MarkedBoard mat) (r, c) n =
    MarkedBoard $
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

-- if i == r )

-- mark :: Board -> MarkedBoard
-- mark board@(Board rows) =
--     let elems = [(r, c, fromJust (rows !! r !! c)) | c <- [0 .. 8], r <- [0 .. 8], isJust $ rows !! r !! c]
--      in foldr (\(r, c, n) b -> updateMarkedBoard b (r, c) n) initialMarkedBoard elems

-- mark :: Board -> MarkedBoard
-- mark b =
--     MarkedBoard
--         [ [candidates b (r, c) | c <- [0 .. 8]]
--         | r <- [0 .. 8]
--         ]
