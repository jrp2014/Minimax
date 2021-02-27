-- |
-- Copyright: (c) 2021 jrp2014
-- SPDX-License-Identifier: MIT
-- Maintainer: jrp2014 <jrp2014@users.noreply.github.com>
--
-- See README for more info
module Minimax where

import Data.Char (isDigit)
import Data.List
  ( group,
    intercalate,
    sortBy,
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

data Player = O | B | X deriving (Ord, Eq, Show)

otherPlayer :: Player -> Player
otherPlayer O = X
otherPlayer X = O
otherPlayer B = undefined

-- Score things (by Player, so X is the best score)
data Scored a = Scored Player a
  deriving (Show)

instance Eq (Scored a) where
  (Scored p _) == (Scored q _) = p == q

instance Ord (Scored a) where
  compare (Scored p _) (Scored q _) = compare p q

type ScoredBoard = Scored Board

scoreBoard :: Board -> ScoredBoard
scoreBoard board =
  Scored
    ( maximum $
        rowWinner
          <$> [byRow board, byCol board, byDiagonal board, byReverseDiagonal board]
    )
    board
  where
    rowWinner :: [Row] -> Player
    rowWinner b = case winningRun b of
      [] -> B -- no winning run just returns a B
      (w : _) -> w

    -- pull out a run of Os or Xs of sufficient length
    -- this assumes that there is only one, otherwise you will either just get
    -- the first or, concatenations of runs
    winningRun :: [Row] -> [Player]
    winningRun = concat . concatMap (filter (\g -> g == os || g == xs) . group)

    os, xs :: [Player]
    os = replicate win O
    xs = replicate win X

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
  Node
    (Scored bestPlay board)
    scoredNextBoards
  where
    scoredNextBoards :: [Tree ScoredBoard]
    scoredNextBoards = map (mkScoredTree (otherPlayer p)) nextBoards

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
bestNextBoardsForX :: Board -> [ScoredBoard]
bestNextBoardsForX board =
  if null xWins
    then -- bring moves with a path to victory (ie, scored X) to the front
         sortBy (flip compare) $ map rootLabel scoredNextBoards
    else xWins -- if the next move wins, take it
  where
    scoredNextBoards = subForest $ mkGameTree X board
    xWins =
      [Scored X b | Node (Scored X b) _ <- scoredNextBoards, winner b == X]

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


-- MAIN
main :: IO ()
main = do
  let board = test -- or empty
  putStrLn $ "win " ++ show win
  putStrLn $ "depth " ++ show depth
  putStrLn $ "rows " ++ show rows
  putStrLn $ "cols " ++ show cols
  putStrLn $ ""
  play board

-- user entry validation
data Command = Column Int | Quit | Invalid deriving (Eq, Show)

interpret :: String -> Command
interpret "q" = Quit
interpret com
  | all isDigit com =
    let d = read com in if d < cols then Column d else Invalid
interpret _ = Invalid

-- Gameplay
play :: Board -> IO ()
play board = do
  showBoard board
  case winner board of
    O -> putStrLn "You win!"
    X -> putStrLn "I win!"
    B -> do
      putStr "Player O enter your move: "
      command <- getLine
      case interpret command of
        Quit -> return ()
        Invalid -> do
          putStrLn $ command ++ " is invalid.  Enter column number or q"
          play board
        Column c -> do
          case makeMove O c board of
            Nothing -> do
              putStrLn "Invalid move.  That column is full."
              play board
            Just newBoard -> do
              case bestNextBoardsForX newBoard of
                [] -> do
                  putStrLn "Game over. Draw."
                (Scored B b : _) -> do
                  putStrLn "I can't find a path to victory yet"
                  play b
                path@(Scored X b : _) -> do
                  putStrLn "I've found a path to victory!"
--                  mapM_
--                    ( \(Scored s b) -> do
--                        putStrLn $ "\nScore: " ++ [showPlayer s]
--                        showBoard b
--                    )
--                    path
                  play b
                (Scored O b : _) -> do
                  putStrLn "Forced win for you"
                  play b

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

test4 :: Board
test4 = [[X, O, X], [X, X, O], [O, O, X]]

test5 :: Board
test5 =
  [ [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, X, B, O, B, B],
    [B, B, O, X, X, B, B],
    [B, B, O, O, X, B, B],
    [X, O, O, X, X, X, O]
  ]
