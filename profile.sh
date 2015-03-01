#!/bin/sh
cabal clean && cd src && ghc -o Main -O2 --make -fforce-recomp -prof -fprof-auto -fprof-auto-calls -caf-all -auto-all -rtsopts Main.hs && time ./Main +RTS -p -sstderr && rm Main *.o *.hi

