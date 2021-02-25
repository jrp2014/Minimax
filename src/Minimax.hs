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
import Data.Maybe
import Data.Ord
import Data.Tree

projectName :: String
projectName = "minimax"

rows :: Int
rows = 6

cols :: Int
cols = 7

win :: Int
win = 3

depth :: Int
depth = 3

type Board = [Row]

type Row = [Player]

data Player = O | B | X deriving (Ord, Eq, Show)

otherPlayer :: Player -> Player
otherPlayer O = X
otherPlayer X = O
otherPlayer B = undefined

empty :: Board
empty = replicate rows $ replicate cols O

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

byDiagonal' :: Board -> [Row]
byDiagonal' [] = []
byDiagonal' ([] : xss) = xss
byDiagonal' xss =
  zipWith
    (++)
    (map ((: []) . head) xss ++ repeat [])
    ([] : byDiagonal' (map tail xss))

type ScoredBoard = (Board, Player)

scoreBoard :: Board -> Player
scoreBoard board =
  maximum $ winner' <$> [byRow board, byCol board, byDiagonal board]
  where
    winner' :: Board -> Player
    winner' b = case runs b of
      [] -> B
      (w : _) -> w

    runs :: Board -> [Player]
    runs = concat . concatMap (filter (\g -> g == os || g == xs) . group)

    os, xs :: [Player]
    os = replicate win O
    xs = replicate win X

mkTree :: Player -> Board -> Tree Board
mkTree p b = Node b (map (mkTree (otherPlayer p)) (expandBoardByCol p b))

prunedepth :: Int -> Tree a -> Tree a
prunedepth n (Node x ts)
  | n > 0 = Node x (map (prunedepth (n - 1)) ts)
  | otherwise = Node x []

mkScoredTree :: Player -> Tree Board -> Tree ScoredBoard
mkScoredTree p (Node board []) = Node (board, scoreBoard board) []
mkScoredTree p (Node board nextBoards) =
  Node
    (board, bestPlay)
    scoredNextBoards
  where
    scoredNextBoards :: [Tree ScoredBoard]
    scoredNextBoards = map (mkScoredTree p) nextBoards

    bestPlay :: Player
    bestPlay =
      (if p == O then minimum else maximum) $
        map (\(Node (_, s) _) -> s) scoredNextBoards

mkGameTree :: Player -> Board -> Tree ScoredBoard
mkGameTree p = mkScoredTree p . mkTree p

bestMove :: Player -> Board -> [Board]
bestMove p b = map (\(Node (b, _) _) -> b) bestMoves
  where
    Node _ nextMoves = mkGameTree p b

    bestMoves = filter (\(Node (_, s) _) -> p == s) nextMoves

minimax :: [Tree ScoredBoard] -> ScoredBoard
minimax = maximumBy (comparing snd) . map minscore
  where
    minscore (Node x []) = x
    minscore (Node (board, _) nextBoards) =
      let (_, score) = minimax nextBoards in (board, score)

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

-- put a play into the last B in the column, if there is one
fillCol :: Player -> Row -> Maybe Row
fillCol p = subst . dropWhile ((/= B) . fst) . picks . reverse
  where
    subst :: [(Player, (Row, Row))] -> Maybe Row
    subst [] = Nothing
    subst ((_, (before, after)) : _) = Just $ reverse $ before ++ p : after

-- The possible next moves for player p
expandBoardByCol :: Player -> Board -> [Board]
expandBoardByCol p = map transpose . mapMaybe toBoard . picks . transpose
  where
    toBoard :: (Row, (Board, Board)) -> Maybe Board
    toBoard (col, (before, after)) = do
      newCol <- fillCol p col
      pure $ before ++ newCol : after

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
test3 = [[B, B, X], [B, X, O], [O, O, X]]
