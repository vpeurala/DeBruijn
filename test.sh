#!/bin/sh
cabal clean && cabal configure --enable-tests --enable-coverage && cabal build && cabal test

