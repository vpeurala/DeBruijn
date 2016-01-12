module Main where

import DeBruijn

main :: IO ()
main = do
  putStrLn $ deBruijnString [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] 4
  putStrLn $ deBruijnString ["I", "II", "III", "IV", "V", "VI", "VII"] 2

