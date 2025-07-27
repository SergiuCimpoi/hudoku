module Infer () where

import Data.Matrix
import Data.Maybe (fromJust, isJust)
import Lib (Board (..))

-- inferences

type Candidates = [Int]
newtype MarkedBoard = MarkedBoard [[Candidates]] deriving (Eq)

candidates :: Board -> (Int, Int) -> Candidates
candidates b (r, c) = undefined

fullMarkedBoard :: MarkedBoard
fullMarkedBoard = MarkedBoard [[[1 .. 9] | _c <- [0 :: Int .. 8]] | _r <- [0 :: Int .. 8]]

updateMarkedBoard :: MarkedBoard -> (Int, Int) -> Int -> MarkedBoard
updateMarkedBoard board (r, c) n = undefined

mark :: Board -> MarkedBoard
mark board@(Board rows) =
    let elems = [(r, c, fromJust (rows !! r !! c)) | c <- [0 .. 8], r <- [0 .. 8], isJust $ rows !! r !! c]
     in foldr (\(r, c, n) b -> updateMarkedBoard b (r, c) n) fullMarkedBoard elems

-- mark :: Board -> MarkedBoard
-- mark b =
--     MarkedBoard
--         [ [candidates b (r, c) | c <- [0 .. 8]]
--         | r <- [0 .. 8]
--         ]
