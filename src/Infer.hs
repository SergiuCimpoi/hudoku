module Infer (MarkingBoard (..), mark, reduceSingles, digitsToCellIds, rowToCells, colToCells, blockToCells, markV, reduceSinglesV) where

import Base (Board (..))
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import Data.List (find, tails)
import qualified Data.Matrix as M
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

data MarkingCell = Value Int | Candidates [Int] deriving (Eq)

--------------
newtype MarkingBoardV = MarkingBoardV (V.Vector MarkingCell) deriving (Eq, Show)

matrixSize :: Int
matrixSize = 9

matrixToVector :: M.Matrix MarkingCell -> MarkingBoardV
matrixToVector mat =
    MarkingBoardV $
        V.fromList [M.getElem r c mat | r <- [1 .. matrixSize], c <- [1 .. matrixSize]]

vectorToMatrix :: MarkingBoardV -> M.Matrix MarkingCell
vectorToMatrix (MarkingBoardV vec) =
    M.matrix matrixSize matrixSize $ \(r, c) ->
        vec V.! ((r - 1) * matrixSize + (c - 1))

toMarkingBoardV :: MarkingBoard -> MarkingBoardV
toMarkingBoardV (MarkingBoard mat) = matrixToVector mat

toMarkingBoard :: MarkingBoardV -> MarkingBoard
toMarkingBoard mbv = MarkingBoard (vectorToMatrix mbv)

addValueST :: MarkingBoardV -> (Int, Int) -> Int -> MarkingBoardV
addValueST (MarkingBoardV vec) (r, c) n = MarkingBoardV $ runST $ do
    mvec <- V.thaw vec
    let index (row, col) = row * 9 + col
        pos = index (r, c)
        inAffected (mr, mc) =
            mr == r || mc == c || isPosInBlock (mr, mc) (blockFromPos (r, c))

    MV.write mvec pos (Value n)

    mapM_
        ( \i -> do
            cell <- MV.read mvec i
            case cell of
                Value _ -> return ()
                Candidates cs -> do
                    let (mr, mc) = i `divMod` 9
                    when (inAffected (mr, mc)) $
                        MV.write mvec i (Candidates (filter (/= n) cs))
        )
        [0 .. 80]

    V.freeze mvec

markV :: Board -> MarkingBoardV
markV (Board mat) =
    let elems = [(r, c, fromJust (mat M.! (r + 1, c + 1))) | c <- [0 .. 8], r <- [0 .. 8], isJust $ mat M.! (r + 1, c + 1)]
        initialV = toMarkingBoardV initialMarkedBoard
     in foldr (\(r, c, n) b -> addValueST b (r, c) n) initialV elems

reduceSinglesV :: MarkingBoardV -> MarkingBoardV
reduceSinglesV boardV@(MarkingBoardV vec) =
    let getCell r c = vec V.! (r * 9 + c)
        elems = [(r, c, getSingle (getCell r c)) | c <- [0 .. 8], r <- [0 .. 8], isSingle (getCell r c)]
     in foldr (\(r, c, n) b -> addValueST b (r, c) n) boardV elems

--------------------
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
blockFromPos (r, c) = (r `div` 3, c `div` 3)

isPosInBlock :: (Int, Int) -> (Int, Int) -> Bool
isPosInBlock (r, c) (br, bc) = r >= startRow && r < startRow + 3 && c >= startCol && c < startCol + 3
  where
    startRow = 3 * br
    startCol = 3 * bc

addValue :: MarkingBoard -> (Int, Int) -> Int -> MarkingBoard
addValue (MarkingBoard mat) (r, c) n =
    MarkingBoard $
        M.matrix
            9
            9
            ( \(mr, mc) ->
                if mr == (r + 1) && mc == (c + 1)
                    then
                        Value n
                    else case mat M.! (mr, mc) of
                        Value v -> Value v
                        Candidates cs ->
                            Candidates $
                                if mr == (r + 1) || mc == (c + 1) || isPosInBlock (mr - 1, mc - 1) (blockFromPos (r, c))
                                    then filter (/= n) cs
                                    else cs
            )

mark :: Board -> MarkingBoard
mark (Board mat) =
    let elems = [(r, c, fromJust (mat M.! (r + 1, c + 1))) | c <- [0 .. 8], r <- [0 .. 8], isJust $ mat M.! (r + 1, c + 1)]
     in foldr (\(r, c, n) b -> addValue b (r, c) n) initialMarkedBoard elems

reduceSingles :: MarkingBoard -> MarkingBoard
reduceSingles board@(MarkingBoard mat) =
    let elems = [(r, c, getSingle (mat M.! (r + 1, c + 1))) | c <- [0 .. 8], r <- [0 .. 8], isSingle $ mat M.! (r + 1, c + 1)]
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
rowToCells (MarkingBoard mat) row = V.toList $ M.getRow (row + 1) mat

colToCells :: MarkingBoard -> Int -> [MarkingCell]
colToCells (MarkingBoard mat) col = V.toList $ M.getCol (col + 1) mat

blockToCells :: MarkingBoard -> Int -> Int -> [MarkingCell]
blockToCells (MarkingBoard mat) i j = M.toList $ M.submatrix startRow (startRow + 2) startCol (startCol + 2) mat
  where
    startRow = 3 * i + 1
    startCol = 3 * j + 1

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
