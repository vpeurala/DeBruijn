#!/bin/sh
cabal clean && cd src && ghc -o Main -O2 --make -fforce-recomp -prof -caf-all -auto-all Main.hs && time ./Main +RTS -p && rm Main && rm *.o && rm *.hi

