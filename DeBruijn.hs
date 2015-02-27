module DeBruijn where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.STRef

main :: IO ()
main = do
  putStrLn "Foo"
  let arr = arrayStuff
  putStrLn $ show arr
  putStrLn "Bar"
 
arrayStuff :: Array Int Int
arrayStuff = runSTArray $ do
  arr <- newArray (0, 4) 0
  mutateArray arr
  return arr

mutateArray :: (MArray a e m, Ix i, Num e, Num i) => a i e -> m (a i e)
mutateArray input = do
  writeArray input 1 3
  return input

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
    forM_ [(succ valueAtTMinusPeriod)..(maximum alphabet)] (\j -> do
      writeArray workingArray t j
      gen1 deBruijnSeq workingArray alphabet subSeqLen (t + 1) t)

{--
arrayThing deBruijnSeq alphabet subSeqLen = seq (runST $ do
  workingArray <- newArray (0 :: Int, ((length alphabet) + 1) :: Int) (minimum alphabet)
  gen1 deBruijnSeq workingArray alphabet subSeqLen 1 1
  )
  deBruijnSeq
--}

calculateDeBruijnSeq :: (Enum a, Ord a) => [a] -> Int -> [a]
calculateDeBruijnSeq []       _         = []
calculateDeBruijnSeq alphabet subSeqLen = runST $ do
  deBruijnSeq <- (newSTRef [])
  workingArray <- newArray (0, subSeqLen) (head alphabet)
  gen1 deBruijnSeq workingArray alphabet subSeqLen 1 1
  result <- readSTRef deBruijnSeq
  return $ result ++ take (subSeqLen - 1) result

-- deBruijnSeq alphabet subSeqLen = runST $ join $ calculateDeBruijnSeq alphabet subSeqLen

{--
deBruijn :: [a] -> Int -> Array Int a
deBruijn alphabet k = runSTArray $ do
  let zero         =  head alphabet
  let alphabetSize =  length alphabet
  workingArray     <- newArray (0, alphabetSize) zero
  sequence         <- 
--}
