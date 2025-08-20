module Base (Board (..), findBestEmpty, update, solve) where

import Data.Foldable (minimumBy)
import qualified Data.Matrix as M
import Data.Maybe (isNothing)
import qualified Data.Vector as V

type Cell = Maybe Int
newtype Board = Board
    { getMatrix :: M.Matrix Cell
    }
    deriving (Eq)

instance Show Board where
    show (Board mat) = unlines (fmap showRow (M.toLists mat))
      where
        showRow row = unwords (fmap showCell row)
        showCell Nothing = "."
        showCell (Just n) = show n

hLine :: Board -> Int -> V.Vector Cell
hLine (Board mat) r = M.getRow (r + 1) mat

--
vLine :: Board -> Int -> V.Vector Cell
vLine (Board mat) c = M.getCol (c + 1) mat

block :: Board -> Int -> Int -> M.Matrix Cell
block (Board mat) i j = M.submatrix startRow (startRow + 2) startCol (startCol + 2) mat
  where
    startRow = 3 * i + 1
    startCol = 3 * j + 1

update :: Board -> (Int, Int) -> Cell -> Board
update (Board mat) (r, c) val = Board $ M.setElem val (r + 1, c + 1) mat

possible :: Board -> (Int, Int) -> [Int]
possible board (r, c) =
    [ n
    | n <- [1 .. 9]
    , n `V.notElem` V.catMaybes (hLine board r)
    , n `V.notElem` V.catMaybes (vLine board c)
    , n `V.notElem` V.catMaybes (M.getMatrixAsVector $ block board (r `div` 3) (c `div` 3))
    ]

-- find the empty position (row, col) with least possible numbers
findBestEmpty :: Board -> Maybe ((Int, Int), [Int])
findBestEmpty board@(Board mat) =
    let allEmptyPos = [((r, c), possible board (r, c)) | r <- [0 .. 8], c <- [0 .. 8], isNothing $ mat M.! (r + 1, c + 1)]
     in if null allEmptyPos
            then Nothing
            else Just $ minimumBy (\a b -> (length . snd) a `compare` (length . snd) b) allEmptyPos

solve :: Board -> [Board]
solve board =
    case findBestEmpty board of
        Nothing -> [board]
        Just (pos, ns) ->
            let boards = update board pos . Just <$> ns
             in concatMap solve boards
