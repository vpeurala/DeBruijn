{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.List

import Test.HUnit hiding (Test)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import DeBruijn

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [
    testGroup "A De Bruijn sequence"
    [
      testProperty "Contains each n-length string from alphabet exactly once" prop_containsEveryNLengthString,
      testCase "Contains each n-length string from alphabet exactly once" test_containsEveryNLengthString
    ]
  ]

alphabet10        = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
deBruijnOf4From10 = calculateDeBruijnSeq alphabet10 4
pickingsOf4From10 = pickings 4 alphabet10

prop_containsEveryNLengthString :: SubSeqOf4FromAlphabet10 -> Bool
prop_containsEveryNLengthString (SubSeqOf4FromAlphabet10 subSeq) =
  occurrences subSeq deBruijnOf4From10 == 1

test_containsEveryNLengthString =
  let picksWithOccurrencesOtherThan1 = filter (\c -> occurrences c deBruijnOf4From10 /= 1) [[9, 0, 0, 0], [9, 9, 0, 0], [9, 9, 9, 0]]
      errors                         = map (\c -> "Pick " ++ show c ++ " should have occurred exactly once in sequence " ++ show deBruijnOf4From10 ++ ", but occurred " ++ show (occurrences c deBruijnOf4From10) ++ " times.") picksWithOccurrencesOtherThan1
  in  [] @=? errors

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = [[]]
combinations 1 xs = map (:[]) xs
combinations n (x:xs) =
  map (x:) (combinations (n - 1) xs)
  ++
  (combinations n xs)

pickings :: Int -> [a] -> [[a]]
pickings _ [] = []
pickings 0 _  = [[]]
pickings 1 xs = map (:[]) xs
pickings n xs =
  concatMap (\x -> map (\p -> (x:p)) (pickings (n - 1) xs)) xs

occurrences :: (Eq a) => [a] -> [a] -> Int
occurrences needle [] = 0
occurrences needle haystack =
  if (take (length needle) haystack) == needle
  then 1 + (occurrences needle (tail haystack))
  else occurrences needle (tail haystack)

newtype SubSeqLen = SubSeqLen Int deriving (Enum, Eq, Integral, Num, Ord, Read, Real, Show)

instance Functor Alphabet where
  fmap f (Alphabet xs) = Alphabet (map f xs)

instance (Arbitrary a) => Arbitrary (Alphabet a) where
  arbitrary = Alphabet `fmap` (arbitrary `suchThat` (\xs -> length xs <= 5))

instance Arbitrary SubSeqLen where
  arbitrary = SubSeqLen `fmap` (arbitrary `suchThat` (\v -> v >= 0 && v <= 3))

newtype SubSeqOf4FromAlphabet10 = SubSeqOf4FromAlphabet10 [Int] deriving (Eq, Ord, Read, Show)

instance Arbitrary SubSeqOf4FromAlphabet10 where
  arbitrary = SubSeqOf4FromAlphabet10 `fmap` (vectorOf 4 (elements alphabet10))

