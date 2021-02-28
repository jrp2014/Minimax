{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Char (isDigit)
import Minimax
    ( Scored(Scored),
      Player(O, B, X),
      Board,
      rows,
      cols,
      win,
      depth,
      winner,
      makeMove,
      showBoard,
      bestNextBoards )
import System.IO ( hFlush, stdout )

main :: IO ()
main = do
  let board = test -- or empty
  putStrLn $ "win " ++ show win
  putStrLn $ "depth " ++ show depth
  putStrLn $ "rows " ++ show rows
  putStrLn $ "cols " ++ show cols
  putStrLn ""
  play board

-- user entry validation
data Command = Column Int | Quit | Invalid deriving stock (Eq, Show)

interpret :: String -> Command
interpret "q" = Quit
interpret com
  | com /= "" && all isDigit com =
    let d = read com in if d < cols then Column d else Invalid
interpret _ = Invalid

-- Gameplay
play :: Board -> IO ()
play !board = do
  showBoard board
  case winner board of
    O -> putStrLn "You win!"
    X -> putStrLn "I win!"
    B -> do
      putStr "Player O enter your move: "
      hFlush stdout
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
              case bestNextBoards newBoard X of
                [] -> do
                  putStrLn "Game over. Draw."
                (Scored B b : _) -> do
                  putStrLn "I can't find a path to victory yet"
                  play b
                paths@(Scored X b : _) -> do
                  putStrLn $ show (length paths) ++ " path(s) to victory found!"
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

test6 :: Board
test6 =
  [ [B, B, B, B, B, B, B],
    [B, B, B, B, B, B, B],
    [B, B, B, B, O, B, O],
    [X, B, B, X, X, B, O],
    [X, X, O, O, X, B, O],
    [X, O, O, X, X, X, O]
  ]
