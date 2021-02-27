{-# LANGUAGE DerivingStrategies #-}

-- |
-- Copyright: (c) 2021 jrp2014
-- SPDX-License-Identifier: MIT
-- Maintainer: jrp2014 <jrp2014@users.noreply.github.com>
--
-- See README for more info
module Minimax
  ( Scored (Scored),
    Player (O, B, X),
    Board,
    rows,
    cols,
    win,
    depth,
    winner,
    makeMove,
    showBoard,
    bestNextBoards,
  )
where

import Control.Parallel.Strategies (parMap, rseq)
import Data.List
  ( group,
    intercalate,
    transpose,
  )
import Data.Maybe (mapMaybe)
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

byDiagonal' :: Board -> [Row]
byDiagonal' [] = []
byDiagonal' ([] : xss) = xss
byDiagonal' xss =
  zipWith
    (++)
    (map ((: []) . head) xss ++ repeat [])
    ([] : byDiagonal' (map tail xss))

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
        <$> [byRow board, byCol board, byDiagonal board, byReverseDiagonal board]

rowWinner :: [Row] -> Player
rowWinner b = case winningRun b of
  [] -> B -- no winning run just returns a B
  (w : _) -> w

-- pull out a run of Os or Xs of sufficient length
-- this assumes that there is only one, otherwise you will either just get
-- the first or, concatenations of runs
winningRun :: [Row] -> [Player]
winningRun = concat . concatMap (filter (\g -> g == owin || g == xwin) . group)

owin, xwin :: [Player]
owin = replicate win O
xwin = replicate win X

winner :: Board -> Player
winner b = p where Scored p _ = scoreBoard b

--  Generate game trees
--  These could probably be fused, to avoid traversing the tree several times

mkTree :: Player -> Board -> Tree Board
mkTree p b = Node b (map (mkTree (otherPlayer p)) (expandBoardByCol p b))

-- this is effectively minimax
mkScoredTree :: Player -> Tree Board -> Tree ScoredBoard
mkScoredTree _ (Node board []) = Node (scoreBoard board) []
mkScoredTree p (Node board nextBoards) =
  if w /= B
    then Node (Scored w board) []
    else Node (Scored bestPlay board) scoredNextBoards
  where
    w = winner board

    scoredNextBoards :: [Tree ScoredBoard]
    scoredNextBoards = parMap rseq (mkScoredTree (otherPlayer p)) nextBoards

    Scored bestPlay _ =
      (if p == O then minimum else maximum) (map rootLabel scoredNextBoards)

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
-- Thic could be optimized to avoid having to rebuild the game tree each time
bestNextBoards :: Board -> Player -> [ScoredBoard]
bestNextBoards board player =
  if null playerWins
    then -- bring moves with a path to victory (ie, scored X) to the front
    -- sortBy (flip compare) $ map rootLabel scoredNextBoards
      [sb | (Node sb@(Scored p _) _) <- scoredNextBoards, p == best]
    else playerWins -- if the next move wins, take it
  where
    Node (Scored best _) scoredNextBoards = mkGameTree player board
    playerWins = [sb | Node sb@(Scored _ b) _ <- scoredNextBoards, winner b == player]

-- handy utility
picks :: [x] -> [(x, ([x], [x]))] -- [(x-here, ([x-before], [x-after]))]
picks [] = []
picks (x : xs) =
  (x, ([], xs)) : [(y, (x : ys, zs)) | (y, (ys, zs)) <- picks xs]

{-
-- The possible next moves with a row
expandRow :: Player -> Row -> [Row]
expandRow p = map toRow . filter ((== B) . fst) . picks
  where
    toRow :: (Player, (Row, Row)) -> Row
    toRow (_, (before, after)) = before ++ p : after

-- The possible next moves for player p
expandBoardByRow :: Player -> Board -> [Board]
expandBoardByRow p = concatMap toBoards . picks
  where
    toBoards :: (Row, (Board, Board)) -> [Board]
    toBoards (row, (before, after)) =
      [before ++ r : after | r <- expandRow p row, validRow r after]

    -- Rows where having a player with a blank in the column
    -- underneath are not valid
    validRow :: Row -> Board -> Bool
    validRow row rows =
      all (\(p, c) -> (p == B) || (B `notElem` c)) $ zip row (transpose rows)
-}

-- put a play into the last B in the column, if there is one
fillCol :: Player -> Row -> Maybe Row
fillCol p = subst . dropWhile ((/= B) . fst) . picks . reverse
  where
    subst :: [(Player, (Row, Row))] -> Maybe Row
    subst [] = Nothing
    subst ((_, (before, after)) : _) = Just $ reverse $ before ++ p : after

-- The possible next moves for player p
expandBoardByCol :: Player -> Board -> [Board]
expandBoardByCol p = map byCol . mapMaybe toBoard . picks . byCol
  where
    toBoard :: (Row, (Board, Board)) -> Maybe Board
    toBoard (col, (before, after)) = do
      newCol <- fillCol p col
      pure $ before ++ newCol : after

-- try to add a player to the nth column
makeMove :: Player -> Int -> Board -> Maybe Board
makeMove p n b = do
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
