module DeBruijn (deBruijnSequence, deBruijnString) where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.List (nub, sort)
import Data.STRef

deBruijnSequence :: (Enum a, Ord a) => [a] -> Int -> [a]
deBruijnSequence alphabet subSequenceLength = calculateDeBruijnSeq (nub $ sort alphabet) subSequenceLength

deBruijnString :: (Enum a, Ord a, Show a) => [a] -> Int -> String
deBruijnString alphabet subSequenceLength = concat $ map show $ deBruijnSequence alphabet subSequenceLength

-- See "An Efficient Algorithm for Generating Necklaces with Fixed Density" by Joe Sawada and Frank Ruskey
gen1 :: (Ord e, Enum e) => STRef s [e] -> STArray s Int e -> [e] -> Int -> Int -> Int -> ST s ()
gen1 deBruijnSeq workingArray alphabet subSeqLen t period =
  if t > subSeqLen
  then
    if (subSeqLen `mod` period == 0)
    then do
      deBruijnSeq' <- readSTRef deBruijnSeq
      workingArray' <- getElems workingArray
      writeSTRef deBruijnSeq (deBruijnSeq' ++ (reverse $ take period $ reverse workingArray'))
    else do
      return ()
  else do
    valueAtTMinusPeriod <- readArray workingArray (t - period)
    writeArray workingArray t valueAtTMinusPeriod
    gen1 deBruijnSeq workingArray alphabet subSeqLen (t + 1) period
    forM_ (successors alphabet valueAtTMinusPeriod) (\j -> do
      writeArray workingArray t j
      gen1 deBruijnSeq workingArray alphabet subSeqLen (t + 1) t)

successors :: (Ord a) => [a] -> a -> [a]
successors alphabet letter =
  dropWhile (\l -> l <= letter) alphabet 

calculateDeBruijnSeq :: (Enum a, Ord a) => [a] -> Int -> [a]
calculateDeBruijnSeq []       _         = []
calculateDeBruijnSeq alphabet subSeqLen = runST $ do
  deBruijnSeq <- (newSTRef [])
  workingArray <- newArray (0, subSeqLen) (minimum alphabet)
  gen1 deBruijnSeq workingArray alphabet subSeqLen 1 1
  result <- readSTRef deBruijnSeq
  return $ result ++ take (subSeqLen - 1) result

