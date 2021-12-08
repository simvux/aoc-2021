{-# LANGUAGE NamedFieldPuns #-}

import Data.List.Split
import Data.Char
import Data.List (sortOn)
import Control.Arrow ((&&&))
import qualified Data.Set as Set
import Data.Set ((\\))

main = do
   input <- parse <$> getContents
   print (part1 input)
   print (part2 input)

data Entry = Entry
  { signals :: [String]
  , output   :: [String]
  } deriving (Show)

parse :: String -> [Entry]
parse = map (collect . map words . splitOn " | ") . lines
 where collect [signals, output] = Entry { signals, output }

part1 :: [Entry] -> Int
part1 = foldr check 0
 where check entry acc = acc + length (filter simples (output entry))

simples word = length word `elem` [2,4,3,7]

part2 :: [Entry] -> Int
part2 = sum . map (digitsToInt . solveEntry)

type Translation = String -> (Bool, Bool)

resolveDigit :: Translation -> String -> Int
resolveDigit fp digit = resolve (length digit)
 where
  resolve 5 =
   case fp digit of
    (True, _) -> 5
    (_, True) -> 2
    _ -> 3
  resolve 6 =
   case fp digit of
    (True, True) -> 6
    (True, False) -> 9
    _ -> 0
  resolve other =
   case other of
    2 -> 1
    3 -> 7
    4 -> 4
    _ -> 8

translation :: [String] -> Translation
translation s =
  let
    [d1, d7, d4, d8] = map Set.fromList $ sortOn length $ filter simples s
    bd = d4 \\ d1
    eg = d8 \\ foldl1 Set.union [d1, d4, d7]
  in (Set.isSubsetOf bd &&& Set.isSubsetOf eg) . Set.fromList

solveEntry :: Entry -> [Int]
solveEntry entry =
  let fp = translation (signals entry)
   in map (resolveDigit fp) (output entry)

digitsToInt :: [Int] -> Int
digitsToInt digits = read (map intToDigit digits)
