{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: (c) 2021 jrp2014
-- SPDX-License-Identifier: MIT
-- Maintainer: jrp2014 <jrp2014@users.noreply.github.com>
--
-- See README for more info
module Minimax
  ( projectName,
  )
where

import Data.List

projectName :: String
projectName = "minimax"

rows :: Int
rows = 6

cols :: Int
cols = 7

win :: Int
win = 4

depth :: Int
depth = 6

type Board = [Row]

type Row = [Player]

data Player = O | B | X | Y | Z deriving (Ord, Eq, Show)

byRow :: Board -> [Row]
byRow = id

byCol :: Board -> [Row]
byCol = transpose

picks :: [x] -> [(x, ([x], [x]))] -- [(x-here, ([x-before], [x-after]))]
picks [] = []
picks (x : xs) =
  (x, ([], xs)) : [(y, (x : ys, zs)) | (y, (ys, zs)) <- picks xs]

-- The possible next moves with a row
expandRow :: Player -> Row -> [Row]
expandRow p = map toRow . filter ((== B) . fst) . picks
  where
    toRow :: (Player, (Row, Row)) -> Row
    toRow (_, (before, after)) = before ++ p : after

-- The possible next moves for player p
expandBoard :: Player -> Board -> [Board]
expandBoard p = concatMap toBoards . picks
  where
    toBoards :: (Row, (Board, Board)) -> [Board]
    toBoards (row, (before, after)) =
      [ before ++ r : after
        | r <- expandRow p row,
          validRow r after
      ]

    -- Rows where having a player with a blank in the column
    -- underneath are not valid
    validRow :: Row -> Board -> Bool
    validRow row rows =
      all (\(p, c) -> (p == B) || (B `notElem` c)) $ zip row (transpose rows)

expand :: Player -> Board -> Board
expand player board = rows1 ++ [row1 ++ player : row2] ++ rows2
  where
    (rows1, row : rows2) = span (notElem B) board
    (row1, b : row2) = span (/= B) row

-- No blanks
complete :: Board -> Bool
complete = and . fmap (notElem B)

-- Assumes a unique row winner
rowWinner :: Row -> Player
rowWinner r = case filter ((== win) . length) $ group r of
  [] -> O
  [w : _] -> w

showBoard :: Board -> IO ()
showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
  where
    showRow = map showPlayer
    line = replicate cols '-'
    nums = take cols ['0' ..]

showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '.'
showPlayer X = 'X'
showPlayer Y = 'Y'
showPlayer Z = 'Z'

main :: IO ()
main = undefined

test :: Board
test =
  [ [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, X, X, B, B],
    [B, B, O, O, X, B, B],
    [B, O, O, X, X, X, O]
  ]

test2 :: Board
test2 =
  [ [B, B, B, B, B],
    [B, B, B, B, B],
    [B, B, B, X, B],
    [B, B, O, O, B],
    [B, O, O, X, O]
  ]

test3 :: Board
test3 =
  [ [B, B],
    [X, B]
  ]
