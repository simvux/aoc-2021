import System.IO
import Data.Char (digitToInt)
import Data.List (foldl')

type BitCounts = [Int]

main = do
  parsed <- parse <$> getContents
  print (part1 parsed)
  print (part2 parsed)

parse = map (map digitToInt) . lines

mostCommon  = map (\count -> fromEnum (count >= 0))
leastCommon = map (\count -> fromEnum (count < 0))

toDec = foldl' (\acc x -> acc * 2 + x) 0

part1 :: [[Int]] -> Int
part1 = 
   present . collectFullBitCount
  where 
   present counts = toDec (mostCommon counts) * toDec (leastCommon counts)

collectFullBitCount :: [[Int]] -> BitCounts
collectFullBitCount (first:xs) = 
  foldr (zipWith (\n c -> c + calc n)) (map calc first) xs
  where
   calc 0 = -1
   calc 1 = 1

part2 :: [[Int]] -> Int
part2 input = 
  oxygen * co2
 where
  oxygen = findRating mostCommon  0 input 
  co2    = findRating leastCommon 0 input

findRating :: (BitCounts -> [Int]) -> Int -> [[Int]] -> Int
findRating _ _ [row] = toDec row
findRating getToKeep i rows = 
   let filtered = filter apply rows
    in findRating getToKeep (i+1) filtered
  where
   genFilter = getToKeep $ collectFullBitCount rows
   apply :: [Int] -> Bool
   apply rows = (genFilter !! i) == (rows !! i)
