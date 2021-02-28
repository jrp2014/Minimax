cabal build  lib:minimax exe:minimax --enable-profiling --ghc-options="-fprof-auto"
cabal run  exe:minimax --enable-profiling --ghc-options="-fprof-auto"  -- +RTS -p
