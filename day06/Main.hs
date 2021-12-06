{-# LANGUAGE TupleSections #-}

import Data.List.Split
import Data.Array
import Debug.Trace (traceShow, trace)

main :: IO ()
main = do
  fishes <- map read . splitOn "," <$> getContents
  print $ length $ generations 80 fishes

type Fish = Int

generations :: Int -> [Fish] -> [Fish]
generations limit fishes = foldr (\_ acc -> nextGeneration acc) fishes [1..limit]

nextGeneration :: [Fish] -> [Fish]
nextGeneration (0:fishes) = 6 : 8 : nextGeneration fishes
nextGeneration (n:fishes) = n - 1 : nextGeneration fishes
nextGeneration [] = []
