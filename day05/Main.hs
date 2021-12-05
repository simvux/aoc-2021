{-# LANGUAGE TupleSections #-}

import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Vent = Vent (Int, Int) (Int, Int) deriving (Show)

main = do
  input <- parse <$> getContents
  print $ most (part1 input)
  print $ most (part2 input)

part1 = addToGrid False Map.empty

part2 = addToGrid True Map.empty

most = length . filter (> 1) . Map.elems

parse :: String -> [Vent]
parse = map vent . lines
 where
  vent = lines_ . splitOn " -> "
  lines_ [from, to] = Vent (line from) (line to)
  line l = let [x,y] = map read $ splitOn "," l in (x, y)

type Grid = Map (Int, Int) Int

addToGrid :: Bool -> Grid -> [Vent] -> Grid
addToGrid withDiags = foldr go
  where 
    mark = 
      foldr (\key acc -> Map.insertWith (+) key 1 acc)
    go (Vent (x1, y1) (x2, y2)) grid
      | x1 == x2 = mark grid $ map (x1,) (between y1 y2)
      | y1 == y2 = mark grid $ map (,y1) (between x1 x2)
      | otherwise && not withDiags = grid
      | otherwise = 
          drawDiag x1 y1 $ Map.insertWith (+) (x1, y1) 1 grid
            where
              dx = x2 - x1
              dy = y2 - y1
              drawDiag x y gr
                | x == x2   = gr
                | otherwise = 
                   let
                     nx = x + div dx (abs dx)
                     ny = y + div dy (abs dy)
                   in drawDiag nx ny $ Map.insertWith (+) (nx, ny) 1 gr
              
between :: Int -> Int -> [Int]
between from to
  | from < to = [from..to]
  | otherwise = [to..from]
