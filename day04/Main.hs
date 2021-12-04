{-# LANGUAGE TupleSections #-}

import System.IO
import Data.List.Split
import Data.List

main = do
  (drawn, boards) <- parse <$> getContents
  print $ part1 boards drawn
  print $ part2 boards drawn

type Slot = (Int, Bool)

type Board = [[Slot]]

type Drawn = [Int]

parse :: String -> (Drawn, [Board])
parse = all . lines
 where
  all :: [String] -> (Drawn, [Board])
  all (x:xs) = (drawn x, boards $ unlines xs)

  drawn  = map read . splitOn ","

  boards :: String -> [Board]
  boards = map board . splitOn "\n\n"

  board :: String -> Board
  board = map (map readSlot . words) . filter (not . null) . lines

  readSlot word = (read word, False)

sortByVictory :: [Board] -> Drawn -> [(Board, Int)]
sortByVictory boards (drawn:xs) =
   let 
     newBoards = updateBoards drawn boards
     (winners, loosers) = partition hasWon newBoards
   in map (, drawn) winners ++ sortByVictory loosers xs
sortByVictory _ [] = []

updateBoards :: Int -> [Board] -> [Board]
updateBoards drawn = map (updateBoard drawn)

updateBoard :: Int -> Board -> Board
updateBoard drawn = mapBoard (\(num, active) -> (num, num == drawn || active))

hasWon :: Board -> Bool
hasWon board = any rowIsComplete board || anyColumnIsComplete board
  where 
    rowIsComplete = all snd
    anyColumnIsComplete = any (all snd) . transpose

mapBoard :: (Slot -> Slot) -> Board -> Board
mapBoard f = map (map f)

scoreBoard :: (Board, Int) -> Int
scoreBoard (board, won_on) = won_on * sum (map fst $ filter (not . snd) $ concat board)

part1 :: [Board] -> Drawn -> Int
part1 board drawn = 
  scoreBoard $ head $ sortByVictory board drawn

part2 :: [Board] -> Drawn -> Int
part2 board drawn = 
  scoreBoard $ last $ sortByVictory board drawn
