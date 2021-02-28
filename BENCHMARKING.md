# Benchmarking

## Starting point

Profiling is done by

    cabal build  lib:minimax exe:minimax --enable-profiling --ghc-options="-fprof-auto"
    cabal run  exe:minimax --enable-profiling --ghc-options="-fprof-auto"  -- +RTS -p

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

    winningRun :: [Row] -> [Player]
    winningRun = concat . concatMap (filter (\g -> g == owin || g == xwin) . group)


## Use sliding Window

With

    winningRun :: [Row] -> [Player]
    winningRun = concat . concatMap (filter aWin . windows)

    windows :: [Player] -> [[Player]]
    windows = foldr (zipWith (:)) (repeat []) . take win . tails

    aWin :: [Player] -> Bool
    aWin run = (run == oWin) || (run == xWin)


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


## Use `find`  from `Data.List`

    winningRun :: [Row] -> [Player]
    winningRun = concat . mapMaybe (find aWin .  windows)

makes little difference:

            Sun Feb 28 18:59 2021 Time and Allocation Profiling Report  (Final)

               minimax +RTS -N -p -RTS

            total time  =       10.31 secs   (35169 ticks @ 1000 us, 12 processors)
            total alloc = 60,861,688,424 bytes  (excludes profiling overheads)

    COST CENTRE           MODULE    SRC                              %time %alloc

    windows               Minimax   src/Minimax.hs:113:1-60           32.3   56.7
    aWin                  Minimax   src/Minimax.hs:117:1-41           19.4    0.0
    byDiagonal.go         Minimax   src/Minimax.hs:(59,5)-(64,29)     12.8   16.8
    winningRun            Minimax   src/Minimax.hs:110:1-55            7.9    0.1
    scoreBoard.rowWinners Minimax   src/Minimax.hs:(96,5)-(98,81)      7.7    9.5
    expandBoardByCol      Minimax   src/Minimax.hs:(183,1)-(188,37)    6.3    7.1
    byDiagonal.go.ts      Minimax   src/Minimax.hs:64:9-29             3.9    4.4
    ==                    Minimax   src/Minimax.hs:71:46-47            1.8    0.0
    scoreBoard            Minimax   src/Minimax.hs:(91,1)-(98,81)      1.4    0.5
    picks                 Minimax   src/Minimax.hs:(169,1)-(171,65)    1.1    2.3

