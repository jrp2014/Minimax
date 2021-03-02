# Benchmarking

## Starting point

Profiling is done by

```bash
    cabal build  lib:minimax exe:minimax --enable-profiling --ghc-options="-fprof-auto"
    cabal run  exe:minimax --enable-profiling --ghc-options="-fprof-auto"  -- +RTS -p
```

leading to

      Sun Feb 28 15:09 2021 Time and Allocation Profiling Report  (Final)

         minimax +RTS -N -p -RTS

      total time  =       15.31 secs   (52199 ticks @ 1000 us, 12 processors)
      total alloc = 68,963,178,512 bytes  (excludes profiling overheads)

    COST CENTRE           MODULE    SRC                              %time %alloc

    winningRun            Minimax   src/Minimax.hs:128:1-79           49.5   61.7
    winningRun.\          Minimax   src/Minimax.hs:128:48-69          18.4    0.0
    byDiagonal.go         Minimax   src/Minimax.hs:(69,5)-(74,29)      9.6   14.9
    scoreBoard.rowWinners Minimax   src/Minimax.hs:(115,5)-(117,81)    5.4    8.4
    ==                    Minimax   src/Minimax.hs:90:46-47            5.0    0.0
    expandBoardByCol      Minimax   src/Minimax.hs:(194,1)-(199,37)    4.2    6.3
    byDiagonal.go.ts      Minimax   src/Minimax.hs:74:9-29             2.7    3.9
    picks                 Minimax   src/Minimax.hs:(180,1)-(182,65)    0.7    2.1

where

```haskell
    winningRun :: [Row] -> [Player]
    winningRun = concat . concatMap (filter (\g -> g == owin || g == xwin) . group)
```

## Try sliding Window

With

```haskell
    winningRun :: [Row] -> [Player]
    winningRun = concat . concatMap (filter aWin . windows)

    windows :: [Player] -> [[Player]]
    windows = foldr (zipWith (:)) (repeat []) . take win . tails

    aWin :: [Player] -> Bool
    aWin run = (run == oWin) || (run == xWin)
```

we cut running time by a third:

            Sun Feb 28 16:34 2021 Time and Allocation Profiling Report  (Final)

               minimax +RTS -N -p -RTS

            total time  =       10.67 secs   (36376 ticks @ 1000 us, 12 processors)
            total alloc = 62,035,350,232 bytes  (excludes profiling overheads)

    COST CENTRE           MODULE    SRC                              %time %alloc

    windows               Minimax   src/Minimax.hs:133:1-60           31.5   55.7
    aWin                  Minimax   src/Minimax.hs:136:1-41           19.9    0.0
    byDiagonal.go         Minimax   src/Minimax.hs:(70,5)-(75,29)     12.6   16.5
    winningRun            Minimax   src/Minimax.hs:130:1-55            9.9    2.0
    scoreBoard.rowWinners Minimax   src/Minimax.hs:(116,5)-(118,81)    7.4    9.3
    expandBoardByCol      Minimax   src/Minimax.hs:(202,1)-(207,37)    6.1    6.9
    byDiagonal.go.ts      Minimax   src/Minimax.hs:75:9-29             3.6    4.3
    ==                    Minimax   src/Minimax.hs:91:46-47            1.6    0.0
    scoreBoard            Minimax   src/Minimax.hs:(111,1)-(118,81)    1.4    0.5
    picks                 Minimax   src/Minimax.hs:(188,1)-(190,65)    1.1    2.3

## Use prefix search

```haskell
    rowWinner :: [Row] -> Player
    rowWinner rows
      |   any (any (xWin `isPrefixOf`)) tr = X
      |   any (any (oWin `isPrefixOf`)) tr = O
      |   otherwise = B
      where
        tr = map tails rows
```

is not much better:

            Tue Mar  2 19:18 2021 Time and Allocation Profiling Report  (Final)

               minimax +RTS -N -p -RTS

            total time  =       11.99 secs   (40889 ticks @ 1000 us, 12 processors)
            total alloc = 37,577,512,240 bytes  (excludes profiling overheads)

    COST CENTRE              MODULE    SRC                              %time %alloc

    rowWinner                Minimax   src/Minimax.hs:(109,1)-(114,23)   47.0    0.0
    rowWinner.tr             Minimax   src/Minimax.hs:114:5-23           13.2   37.1
    byDiagonal.go            Minimax   src/Minimax.hs:(58,5)-(63,29)      8.9   19.6
    scoreBoard.rowWinners    Minimax   src/Minimax.hs:(95,5)-(101,13)     8.0   15.6
    expandBoardByCol         Minimax   src/Minimax.hs:(185,1)-(191,37)    6.1   11.5
    ==                       Minimax   src/Minimax.hs:70:46-47            5.7    0.0
    byDiagonal.go.ts         Minimax   src/Minimax.hs:63:9-29             3.4    7.0
    scoreBoard.trimDiagonals Minimax   src/Minimax.hs:106:5-70            1.5    2.1
    scoreBoard               Minimax   src/Minimax.hs:(90,1)-(106,70)     1.3    0.8
    fillCol.(...)            Minimax   src/Minimax.hs:178:5-29            1.0    1.1
    picks                    Minimax   src/Minimax.hs:(195,1)-(197,65)    0.4    1.2

## Use `find` from `Data.List`

```haskell
    rowWinner :: [Row] -> Player
    rowWinner = head . fromMaybe [B] . find aWin . windows . concat

    aWin :: [Player] -> Bool
    aWin run = (run == oWin) || (run == xWin)
```

is still worse than using `isPrefix`


            Tue Mar  2 18:43 2021 Time and Allocation Profiling Report  (Final)

               minimax +RTS -N -p -RTS

            total time  =       13.79 secs   (47023 ticks @ 1000 us, 12 processors)
            total alloc = 90,393,616,792 bytes  (excludes profiling overheads)

    COST CENTRE              MODULE    SRC                              %time %alloc

    windows                  Minimax   src/Minimax.hs:111:1-60           30.5   61.8
    aWin                     Minimax   src/Minimax.hs:114:1-41           24.9    0.0
    rowWinner                Minimax   src/Minimax.hs:108:1-63           12.3   12.7
    byDiagonal.go            Minimax   src/Minimax.hs:(57,5)-(62,29)      6.9    7.9
    scoreBoard.rowWinners    Minimax   src/Minimax.hs:(94,5)-(100,13)     6.6    6.2
    expandBoardByCol         Minimax   src/Minimax.hs:(179,1)-(185,37)    5.1    4.7
    ==                       Minimax   src/Minimax.hs:69:46-47            4.4    0.0
    byDiagonal.go.ts         Minimax   src/Minimax.hs:62:9-29             2.7    2.8
    scoreBoard.trimDiagonals Minimax   src/Minimax.hs:105:5-70            1.3    0.8
    scoreBoard               Minimax   src/Minimax.hs:(89,1)-(105,70)     1.2    0.3

## Use `find` from `Data.List`

```haskell
    rowWinner :: [Row] -> Player
    rowWinner =
      ( \case
          [] -> B
          (w : _) -> w
      )
        . concat
        . mapMaybe (find aWin . windows)
  ```

is not much better than using `filter`

            Tue Mar  2 19:41 2021 Time and Allocation Profiling Report  (Final)

               minimax +RTS -N -p -RTS

            total time  =       10.17 secs   (34672 ticks @ 1000 us, 12 processors)
            total alloc = 56,869,004,312 bytes  (excludes profiling overheads)

    COST CENTRE              MODULE    SRC                              %time %alloc

    windows                  Minimax   src/Minimax.hs:119:1-60           31.7   58.4
    aWin                     Minimax   src/Minimax.hs:122:1-41           20.1    0.0
    byDiagonal.go            Minimax   src/Minimax.hs:(59,5)-(64,29)      9.4   13.0
    scoreBoard.rowWinners    Minimax   src/Minimax.hs:(96,5)-(102,13)     8.5   10.3
    expandBoardByCol         Minimax   src/Minimax.hs:(187,1)-(193,37)    6.8    7.6
    rowWinner                Minimax   src/Minimax.hs:(110,1)-(116,36)    6.5    0.1
    ==                       Minimax   src/Minimax.hs:71:46-47            3.8    0.0
    byDiagonal.go.ts         Minimax   src/Minimax.hs:64:9-29             3.8    4.7
    scoreBoard.trimDiagonals Minimax   src/Minimax.hs:107:5-70            1.7    1.4
    scoreBoard               Minimax   src/Minimax.hs:(91,1)-(107,70)     1.7    0.5
    fillCol.(...)            Minimax   src/Minimax.hs:180:5-29            1.2    0.7
    mkScoredTree.(...)       Minimax   src/Minimax.hs:(149,5)-(151,65)    1.0    0.4

