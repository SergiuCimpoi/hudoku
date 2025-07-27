module Data (
    s439,
    aiEscargot,
    platinumBlonde,
    emptyBoard,
    evilSample,
    valid,
) where

import qualified Data.Matrix as M
import Lib_1 (Board (..))

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

emptyBoard :: Board
emptyBoard =
    Board $
        M.fromLists
            [ [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            ]

aiEscargot :: Board
aiEscargot =
    Board $
        M.fromLists
            [ [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 1]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Just 1, Just 2, Nothing, Nothing]
            , [Nothing, Nothing, Just 8, Nothing, Nothing, Nothing, Nothing, Just 3, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Just 2, Nothing, Just 1, Nothing, Just 6, Nothing, Just 7, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Just 6, Nothing, Nothing, Nothing, Nothing, Just 9, Nothing, Nothing]
            , [Nothing, Nothing, Just 3, Just 6, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Just 1, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            ]

platinumBlonde :: Board
platinumBlonde =
    Board $
        M.fromLists
            [ [Nothing, Nothing, Just 5, Nothing, Nothing, Nothing, Just 2, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Just 1, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 5]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Just 9, Nothing, Nothing, Nothing]
            , [Just 4, Nothing, Nothing, Nothing, Just 5, Nothing, Nothing, Nothing, Just 1]
            , [Nothing, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Just 2, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] -- Written as Just 2 below!
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Just 9, Nothing, Nothing]
            ]

evilSample :: Board
evilSample =
    Board $
        M.fromLists
            [ [Nothing, Nothing, Nothing, Nothing, Nothing, Just 3, Nothing, Just 7, Nothing]
            , [Just 1, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 9]
            , [Nothing, Just 8, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
            , [Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Just 6, Nothing, Nothing]
            , [Nothing, Just 6, Nothing, Nothing, Nothing, Nothing, Nothing, Just 2, Nothing]
            , [Nothing, Nothing, Just 2, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing]
            , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 1, Nothing]
            , [Just 6, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just 4]
            , [Nothing, Just 5, Nothing, Just 4, Nothing, Nothing, Nothing, Nothing, Nothing]
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
