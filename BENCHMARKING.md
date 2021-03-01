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
        tr = tails rows
```

more than halves run time:

            Sun Feb 28 21:05 2021 Time and Allocation Profiling Report  (Final)

               minimax +RTS -N -p -RTS

            total time  =        4.70 secs   (16017 ticks @ 1000 us, 12 processors)
            total alloc = 13,185,902,528 bytes  (excludes profiling overheads)

    COST CENTRE              MODULE    SRC                              %time %alloc

    rowWinner                Minimax   src/Minimax.hs:(108,1)-(111,19)   45.1    0.0
    expandBoardByCol         Minimax   src/Minimax.hs:(192,1)-(198,37)   14.4   33.7
    scoreBoard.rowWinners    Minimax   src/Minimax.hs:(95,5)-(101,13)     9.0   17.3
    byDiagonal.go            Minimax   src/Minimax.hs:(58,5)-(63,29)      7.6   20.2
    ==                       Minimax   src/Minimax.hs:70:46-47            6.3    0.0
    scoreBoard.trimDiagonals Minimax   src/Minimax.hs:105:5-70            2.8    3.9
    picks                    Minimax   src/Minimax.hs:(178,1)-(180,65)    2.8   10.8
    scoreBoard               Minimax   src/Minimax.hs:(90,1)-(105,70)     2.4    1.3
    mkScoredTree.(...)       Minimax   src/Minimax.hs:(149,5)-(151,65)    1.4    1.0
    fillCol                  Minimax   src/Minimax.hs:(184,1)-(188,75)    1.3    1.0
    byDiagonal.go.ts         Minimax   src/Minimax.hs:63:9-29             1.0    2.5
    expandBoardByCol.toBoard Minimax   src/Minimax.hs:(195,5)-(198,37)    0.9    1.3
    fillCol.subst            Minimax   src/Minimax.hs:(187,5)-(188,75)    0.9    2.4
    byReverseDiagonal        Minimax   src/Minimax.hs:66:1-40             0.7    1.1
    mkTree                   Minimax   src/Minimax.hs:135:1-73            0.6    1.1


## Use `find` from `Data.List` and simplify `fillCol`

```haskell
    rowWinner :: [Row] -> Player
    rowWinner  = head . fromMaybe [B] . find aWin

    aWin :: [Player] -> Bool
    aWin run = (run == oWin) || (run == xWin)


    fillCol :: Player -> Row -> Maybe Row
    fillCol p r = subst bs rest
      where
        (bs, rest) = span (==B) r

        subst [] _ = Nothing -- the column is full
        subst xs ys = Just $ init xs ++ p : ys
```

leads to a further significant performance improvement:


            Mon Mar  1 19:25 2021 Time and Allocation Profiling Report  (Final)

               minimax +RTS -N -p -RTS

            total time  =        2.09 secs   (7135 ticks @ 1000 us, 12 processors)
            total alloc = 12,672,760,376 bytes  (excludes profiling overheads)

    COST CENTRE              MODULE    SRC                              %time %alloc

    expandBoardByCol         Minimax   src/Minimax.hs:(191,1)-(197,37)   22.9   35.1
    aWin                     Minimax   src/Minimax.hs:114:1-41           19.4    0.0
    scoreBoard.rowWinners    Minimax   src/Minimax.hs:(97,5)-(103,13)    14.4   18.8
    byDiagonal.go            Minimax   src/Minimax.hs:(60,5)-(65,29)     10.9   21.4
    rowWinner                Minimax   src/Minimax.hs:111:1-45            5.1    0.0
    scoreBoard.trimDiagonals Minimax   src/Minimax.hs:108:5-70            4.3    4.2
    scoreBoard               Minimax   src/Minimax.hs:(92,1)-(108,70)     3.4    1.3
    fillCol.(...)            Minimax   src/Minimax.hs:179:5-29            3.3    3.2
    ==                       Minimax   src/Minimax.hs:72:46-47            2.4    0.0
    byDiagonal.go.ts         Minimax   src/Minimax.hs:65:9-29             1.9    2.6
    fillCol.subst'           Minimax   src/Minimax.hs:(181,5)-(182,43)    1.9    2.6
    mkScoredTree.(...)       Minimax   src/Minimax.hs:(141,5)-(143,65)    1.6    1.1
    picks                    Minimax   src/Minimax.hs:(170,1)-(172,65)    1.5    3.5
    expandBoardByCol.toBoard Minimax   src/Minimax.hs:(194,5)-(197,37)    1.2    1.3
    mkTree                   Minimax   src/Minimax.hs:127:1-73            1.0    1.1
    byReverseDiagonal        Minimax   src/Minimax.hs:68:1-40             0.9    1.1


This could probably be improved further by using a better string matching
algorithm.  
