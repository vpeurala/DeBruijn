{-|
Module        : DeBruijn
Description   : Generates <http://en.wikipedia.org/wiki/De_Bruijn_sequence De Bruijn sequences>.
Copyright     : (c) Ville Peurala, 2015
License       : Apache License 2.0
Maintainer    : ville.peurala@gmail.com
Stability     : experimental
Portability   : POSIX

This module generates <http://en.wikipedia.org/wiki/De_Bruijn_sequence De Bruijn sequences>.
-} 
module DeBruijn (deBruijnSequence, deBruijnString) where

import Control.Monad
import Control.Monad.ST
import Data.List (nub, sort)
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- | Generates a <http://en.wikipedia.org/wiki/De_Bruijn_sequence De Bruijn sequence> from alphabet a with the given length of subsequences.
deBruijnSequence :: (Enum a, Ord a) => 
  [a]    -- ^ The alphabet from which the De Bruijn sequence will be generated.
  -> Int -- ^ Length of the subsequences of the De Bruijn sequence.
  -> [a] -- ^ The returned sequence as a 'List'.
deBruijnSequence alphabet = calculateDeBruijnSeq (nub $ sort alphabet)

-- | Generates a 'String' from a <http://en.wikipedia.org/wiki/De_Bruijn_sequence De Bruijn sequence> from alphabet a with the given length of subsequences.
deBruijnString :: (Enum a, Ord a, Show a) => 
  [a]       -- ^ The alphabet from which the De Bruijn sequence will be generated.
  -> Int    -- ^ Length of the subsequences of the De Bruijn sequence.
  -> String -- ^ The returned 'String'.
deBruijnString 
  alphabet
  subSequenceLength
  = concatMap show $ deBruijnSequence alphabet subSequenceLength

-- See "An Efficient Algorithm for Generating Necklaces with Fixed Density" by Joe Sawada and Frank Ruskey
gen1 :: (Ord a) =>
     STRef s (VM.MVector s a)
     -> VM.MVector s a
     -> [a]
     -> Int
     -> Int
     -> Int
     -> ST s ()
gen1 deBruijnSeq workingArray alphabet subSeqLen t period =
  if t > subSeqLen
  then
    when (subSeqLen `mod` period == 0) $ do
      deBruijnSeq' <- readSTRef deBruijnSeq
      let appendable = VM.drop (subSeqLen - period + 1) workingArray
      let firstWritableIndex = VM.length deBruijnSeq'
      let appendableLength = VM.length appendable
      grown <- VM.grow deBruijnSeq' appendableLength
      forM_ [0..(appendableLength - 1)] (\i -> do
        item <- VM.read appendable i
        VM.write grown (firstWritableIndex + i) item)
      writeSTRef deBruijnSeq grown
  else do
    valueAtTMinusPeriod <- VM.read workingArray (t - period)
    VM.write workingArray t valueAtTMinusPeriod
    gen1 deBruijnSeq workingArray alphabet subSeqLen (t + 1) period
    forM_ (successors alphabet valueAtTMinusPeriod) (\j -> do
      VM.write workingArray t j
      gen1 deBruijnSeq workingArray alphabet subSeqLen (t + 1) t)

successors :: (Ord a) => [a] -> a -> [a]
successors alphabet letter =
  dropWhile (<= letter) alphabet 

calculateDeBruijnSeq :: (Enum a, Ord a) => [a] -> Int -> [a]
calculateDeBruijnSeq []       _         = []
calculateDeBruijnSeq alphabet subSeqLen = runST $ do
  deBruijnSeqWorking <- V.thaw $ V.fromList []
  deBruijnSeqRef <- newSTRef deBruijnSeqWorking
  workingArray <- VM.replicate (subSeqLen + 1) (minimum alphabet)
  gen1 deBruijnSeqRef workingArray alphabet subSeqLen 1 1
  deBruijnSeqFinal <- readSTRef deBruijnSeqRef
  deBruijnSeqFrozen <- V.freeze deBruijnSeqFinal
  let sequenceWithoutCycling = V.toList deBruijnSeqFrozen
  let sequenceWithCycling = sequenceWithoutCycling ++ take (subSeqLen - 1) sequenceWithoutCycling
  return sequenceWithCycling



