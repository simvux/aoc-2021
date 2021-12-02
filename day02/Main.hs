import System.IO

type Command = (String, Int)

main = do
  input <- getContents
  let coords = parse input
  print $ "part 1 " ++ show (part1 coords)
  print $ "part 2 " ++ show (part2 coords)

parse :: String -> [Command]
parse = map (parseCommand . words) . lines
 where parseCommand [direction, amount] = (direction, read amount)

part1 :: [Command] -> Int
part1 cms = 
  let (horizontal, depth) = travel cms
   in horizontal * depth
 where 
  travel :: [Command] -> (Int, Int)
  travel (("forward", amount):xs) = (amount, 0)  <:> travel xs
  travel (("up"     , amount):xs) = (0, -amount) <:> travel xs
  travel (("down"   , amount):xs) = (0, amount)  <:> travel xs
  travel [] = (0, 0)

part2 :: [Command] -> Int
part2 cms = 
  let (horizontal, depth) = travel cms 0
   in horizontal * depth
 where
  travel :: [Command] -> Int -> (Int, Int)
  travel (("forward", amount):xs) aim = (amount, aim * amount) <:> travel xs aim
  travel (("up",      amount):xs) aim = (0, 0) <:> travel xs (aim - amount)
  travel (("down",    amount):xs) aim = (0, 0) <:> travel xs (aim + amount)
  travel [] _ = (0, 0)

(<:>) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(<:>) (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)
