import qualified Data.Map as Map
import Data.Map (Map, (!), (!?))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char
import Data.List.Split

main = do
  paths <- parse <$> getContents
  print (part1 paths)
  print (part2 paths)

parse :: String -> Map String [String]
parse input = foldr collect Map.empty (lines input)
 where collect s = let [from, to] = splitOn "-" s in Map.insertWith (++) to [from] . Map.insertWith (++) from [to]

part1 :: Map String [String] -> Int
part1 cons = seek Set.empty (cons ! "start")
 where
  seek :: Set String -> [String] -> Int
  seek smallEncounters = sum . map (checkExit smallEncounters)

  checkExit :: Set String -> String -> Int
  checkExit _ "end"   = 1
  checkExit _ "start" = 0
  checkExit smallEncounters exit
    | isLower (head exit) && Set.member exit smallEncounters = 0
    | isLower (head exit) = cont (Set.insert exit smallEncounters)
    | otherwise    = cont smallEncounters
   where cont smalls = maybe 0 (seek smalls) (cons !? exit)

type Visits = (Bool, Set String)

part2 :: Map String [String] -> Int
part2 cons = seek (False, Set.empty) (cons ! "start")
 where
  seek :: Visits -> [String] -> Int
  seek encounters = sum . map (checkExit encounters)

  checkExit :: Visits -> String -> Int
  checkExit _ "end"   = 1
  checkExit _ "start" = 0
  checkExit (dub, encounters) exit
    | isLower (head exit) && Set.member exit encounters && not dub = cont (True, encounters)
    | isLower (head exit) && Set.member exit encounters = 0
    | isLower (head exit)  = cont (dub, Set.insert exit encounters)
    | otherwise            = cont (dub, encounters)
   where cont smalls = maybe 0 (seek smalls) (cons !? exit)
