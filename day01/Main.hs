{-# LANGUAGE FlexibleContexts #-}

import System.IO

main = part1

part1 = do
  input <- getContents
  print . countIncreases $ map read (lines input)
  
countIncreases :: [Int] -> Int
countIncreases = next 1000000000
  where
    next prev (n:xs)
      | n > prev  = 1 + next n xs
      | otherwise = next n xs
    next _ [] = 0

part2 = do
  input <- getContents
  print . countWindowedIncreases $ map read (lines input)

countWindowedIncreases :: [Int] -> Int
countWindowedIncreases = next 100000000
  where
    next :: Int -> [Int] -> Int
    next prev (a:b:c:xs) =
      let sum = a+b+c
       in fromEnum (sum > prev) + next sum (b:c:xs)
    next prev rest =
      fromEnum (sum rest > prev)
