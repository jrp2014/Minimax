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
