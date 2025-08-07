module Infer (MarkingBoard (..), mark, reduceSingles, digitsToCellIds, rowToCells) where

import Base (Board (..))
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.List (find, tails)
import qualified Data.Matrix as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- inferences

data MarkingCell = Value Int | Candidates [Int] deriving (Eq)

instance Show MarkingCell where
    show (Value v) = 'v' : show v
    show (Candidates []) = ""
    show (Candidates (c : cs)) = show c ++ "," ++ show (Candidates cs)

newtype MarkingBoard = MarkingBoard (M.Matrix MarkingCell) deriving (Eq, Show)

isSingle :: MarkingCell -> Bool
isSingle (Candidates [_]) = True
isSingle _ = False

getSingle :: MarkingCell -> Int
getSingle (Candidates [l]) = l
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

-- rowHiddenPairs :: MarkingBoard -> Int -> V.Vector ((Int, Int), [Int])
-- rowHiddenPairs (MarkingBoard mat) r =
--     let row = V.toList $ M.getRow r mat
--      in

distinctPairs :: [a] -> [(a, a)]
distinctPairs list = [(x, y) | (x : xs) <- tails list, y <- xs]

-- rowHiddenPairs :: [MarkingCell] -> V.Vector ((Int, Int), [Int])
-- rowHiddenPairs list@(cell : cells) = case cell of
--     Value _ -> rowHiddenPairs cells
--     Candidates cs -> (\c -> findIndices (\a -> )) <$> distinctPairs cs

rowToCells :: MarkingBoard -> Int -> [MarkingCell]
rowToCells (MarkingBoard mat) row = V.toList $ M.getRow row mat

digitsToCellIds' :: [MarkingCell] -> V.Vector [Int]
digitsToCellIds' cells =
    foldr
        ( \(idx, c) acc -> case c of
            Value _ -> acc
            Candidates ns ->
                let ns' = fmap (\x -> x - 1) ns
                 in acc V.// [(x, idx : (acc V.! x)) | x <- ns']
        )
        (V.replicate 9 [])
        (zip [0 ..] cells)

digitsToCellIds :: [MarkingCell] -> V.Vector [Int]
digitsToCellIds cells = runST $ do
    mv <- MV.replicate 9 []
    forM_ (zip [0 ..] cells) $ \(idx, c) ->
        case c of
            Value _ -> return ()
            Candidates ns -> forM_ ns $ \x -> do
                old <- MV.read mv (x - 1)
                MV.write mv (x - 1) (idx : old)
    V.freeze mv

-- reduceHiddenPairs :: MarkingBoard -> MarkingBoard
-- reduceHiddenPairs board@(MarkingBoard mat) =
