import Data.List.Split 
import Data.List

main = do 
  crabs <- map read . splitOn "," <$> getContents
  print (part1 crabs)
  print (part2 crabs)

part1 crabs = foldr (\crab acc -> acc + abs (crab - median crabs)) 0 crabs
part2 crabs = foldr (\crab acc -> acc + sum [1..abs $ crab - average crabs]) 0 crabs

average nums = div (sum nums) (length nums)
median  nums = sort nums !! div (length nums) 2
