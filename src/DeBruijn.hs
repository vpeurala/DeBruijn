module DeBruijn (deBruijnSequence, deBruijnString) where

import Control.Monad
import Control.Monad.ST
import Data.List (nub, sort)
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

deBruijnSequence :: (Enum a, Ord a) => [a] -> Int -> [a]
deBruijnSequence alphabet subSequenceLength = calculateDeBruijnSeq (nub $ sort alphabet) subSequenceLength

deBruijnString :: (Enum a, Ord a, Show a) => [a] -> Int -> String
deBruijnString alphabet subSequenceLength = concat $ map show $ deBruijnSequence alphabet subSequenceLength

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
    if (subSeqLen `mod` period == 0)
    then do
      deBruijnSeq' <- readSTRef deBruijnSeq
      let appendable = VM.drop (subSeqLen - period + 1) workingArray
      let firstWritableIndex = VM.length deBruijnSeq'
      let appendableLength = VM.length appendable
      grown <- VM.grow deBruijnSeq' appendableLength
      forM_ [0..(appendableLength - 1)] (\i -> do
        item <- VM.read appendable i
        VM.write grown (firstWritableIndex + i) item)
      writeSTRef deBruijnSeq grown
    else return ()
  else do
    valueAtTMinusPeriod <- VM.read workingArray (t - period)
    VM.write workingArray t valueAtTMinusPeriod
    gen1 deBruijnSeq workingArray alphabet subSeqLen (t + 1) period
    forM_ (successors alphabet valueAtTMinusPeriod) (\j -> do
      VM.write workingArray t j
      gen1 deBruijnSeq workingArray alphabet subSeqLen (t + 1) t)

successors :: (Ord a) => [a] -> a -> [a]
successors alphabet letter =
  dropWhile (\l -> l <= letter) alphabet 

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



