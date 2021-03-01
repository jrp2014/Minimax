{-# LANGUAGE DerivingStrategies #-}

-- |
-- Copyright: (c) 2021 jrp2014
-- SPDX-License-Identifier: MIT
-- Maintainer: jrp2014 <jrp2014@users.noreply.github.com>
--
-- See README for more info
module Minimax where

import Data.List
  ( find,
    intercalate,
    transpose,
  )
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Tree
  ( Tree (..),
    drawTree,
  )

-- dimensions / limits

rows :: Int
rows = 6

cols :: Int
cols = 7

win :: Int
win = 4

depth :: Int
depth = 7

-- The board

type Board = [Row]

type Row = [Player]

empty :: Board
empty = replicate rows $ replicate cols B

byRow :: Board -> [Row]
byRow = id

byCol :: Board -> [Row]
byCol = transpose

byDiagonal :: Board -> [Row]
byDiagonal = tail . go []
  where
    -- it is critical for some applications that we start producing answers
    -- before inspecting es_
    go b es_ =
      [h | h : _ <- b] : case es_ of
        [] -> transpose ts
        e : es -> go (e : ts) es
      where
        ts = [t | _ : t <- b]

byReverseDiagonal :: Board -> [Row]
byReverseDiagonal = byDiagonal . reverse

-- The players.  The minimax algorithm relies on this ordering

data Player = O | B | X deriving stock (Ord, Eq, Show)

otherPlayer :: Player -> Player
otherPlayer O = X
otherPlayer X = O
otherPlayer B = undefined

-- Score things (by Player, so X is the best score)
data Scored a = Scored Player a
  deriving stock (Show)

instance Eq (Scored a) where
  (Scored p _) == (Scored q _) = p == q

instance Ord (Scored a) where
  compare (Scored p _) (Scored q _) = compare p q

type ScoredBoard = Scored Board

scoreBoard :: Board -> ScoredBoard
scoreBoard board =
  Scored
    (if X `elem` rowWinners then X else if O `elem` rowWinners then O else B)
    board
  where
    rowWinners =
      rowWinner
        <$> [ byRow board,
              byCol board,
              trimDiagonals $ byDiagonal board,
              trimDiagonals $ byReverseDiagonal board
            ]

   -- 1st/last win-1 diagonals can't contain winners
    trimDiagonals :: [Row] -> [Row]
    --trimDiagonals = take (rows + cols - 1 - 2 * (win - 1)) . drop (win - 1)
    trimDiagonals = take (rows + cols + 1 - 2 * win ) . drop (win - 1)

rowWinner :: [Row] -> Player
rowWinner  = head . fromMaybe [B] . find aWin

aWin :: [Player] -> Bool
aWin run = (run == oWin) || (run == xWin)

oWin, xWin :: [Player]
oWin = replicate win O
xWin = replicate win X

winner :: Board -> Player
winner b = p where Scored p _ = scoreBoard b

--  Generate game trees
--  These could probably be fused, to avoid traversing the tree several times

mkTree :: Player -> Board -> Tree Board
mkTree p b = Node b (map (mkTree (otherPlayer p)) (expandBoardByCol p b))

-- this is effectively minimax
mkScoredTree :: Player -> Tree Board -> Tree ScoredBoard
mkScoredTree B _ = error "Cannot make a Scored Tree for B"
mkScoredTree _ (Node board []) = Node (scoreBoard board) []
mkScoredTree p (Node board nextBoards) =
  Node
    (Scored bestPlay board)
    scoredNextBoards
  where
    scoredNextBoards :: [Tree ScoredBoard]
    scoredNextBoards = map (mkScoredTree (otherPlayer p)) nextBoards

    Scored bestPlay _ =
      (if p == O then minimum else maximum) $
        map (rootLabel . mkScoredTree (otherPlayer p)) nextBoards

pruneDepth :: Int -> Tree a -> Tree a
pruneDepth d (Node x ts)
  | d > 0 = Node x (map (pruneDepth (d - 1)) ts)
  | otherwise = Node x []

mkGameTree :: Player -> Board -> Tree ScoredBoard
mkGameTree p = mkScoredTree p . pruneDepth depth . mkTree p

-- core logic: next moves that are a path to victory are scored X
-- if the next move is a win, take it, otherwise sort the next move with those
-- offering a path to victory first.
--
-- This could be optimized to avoid having to rebuild the game tree each time
bestNextBoards :: Board -> Player -> [ScoredBoard]
bestNextBoards board player =
  if null playerWins
    then [sb | (Node sb@(Scored p _) _) <- scoredNextBoards, p == best]
    else playerWins -- if the next move wins, take it
  where
    Node (Scored best _) scoredNextBoards = mkGameTree player board
    playerWins =
      [sb | Node sb@(Scored _ b) _ <- scoredNextBoards, winner b == player]

-- put a play into the last B in the column, if there is one
fillCol :: Player -> Row -> Maybe Row
fillCol p r = subst bs rest
  where
    (bs, rest) = span (==B) r

    subst [] _ = Nothing -- the column is full
    subst xs ys = Just $ init xs ++ p : ys

-- The possible next moves for player p, central moves prioritised
expandBoardByCol :: Player -> Board -> [Board]
expandBoardByCol p = map byCol . mapMaybe toBoard . reorder . picks . byCol
  where
    toBoard :: (Row, (Board, Board)) -> Maybe Board
    toBoard (col, (before, after)) = do
      -- the Maybe Monad
      newCol <- fillCol p col
      pure $ before ++ newCol : after

-- handy utility
picks :: [x] -> [(x, ([x], [x]))] -- [(x-here, ([x-before], [x-after]))]
picks [] = []
picks (x : xs) =
  (x, ([], xs)) : [(y, (x : ys, zs)) | (y, (ys, zs)) <- picks xs]

-- reorder a list so that central elements come first
reorder :: [a] -> [a]
reorder list = interweave (reverse before) after
  where
    (before, after) = splitAt (length list `div` 2) list

    interweave [] ys = ys
    interweave xs [] = xs
    interweave (x : xs) (y : ys) = y : x : interweave xs ys

-- try to add a player to the nth column
makeMove :: Player -> Int -> Board -> Maybe Board
makeMove p n b = do
  -- Maybe Monad
  newCol <- fillCol p (head after)
  return $ byCol $ before ++ newCol : tail after
  where
    (before, after) = splitAt n (byCol b)

showBoard :: Board -> IO ()
showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
  where
    showRow = map showPlayer
    line = replicate cols '='
    nums = take cols ['0' ..]

showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '.'
showPlayer X = 'X'

-- for debugging
drawScoredTree :: Tree ScoredBoard -> String
drawScoredTree = drawTree . fmap showScored
  where
    showScored :: ScoredBoard -> String
    showScored (Scored s b) =
      showPlayer s : '+' : intercalate "\n  " (map (map showPlayer) b)
