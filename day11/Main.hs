{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.Vector as Vec
import Data.Vector hiding ((++))
import Data.Char
import Data.Maybe
import Debug.Trace (traceShow, trace)
import Data.Set as Set

main = do
  octi <- parse <$> getContents
  print (part1 octi)

part1 :: Octi -> Int
part1 octi =
  fst
  $ Prelude.foldr
    (\_ (count, octi_) ->
      let (ncount, nocti) = step octi_
       in trace ("old: " ++ show octi_ ++ "\nnew: " ++ show nocti) (ncount + count, nocti)
    )
    (0, octi)
    [0..0]

type Octi = Vector (Vector Int)

parse :: String -> Octi
parse input = Vec.fromList $ Prelude.map parseLine $ lines input
 where parseLine line = Vec.fromList $ Prelude.map digitToInt line

data Position = Position 
  { row :: Int
  , column :: Int
  , octi :: Octi
  , flashes :: Set (Int, Int)
  } deriving (Show)

step :: Octi -> (Int, Octi)
step octi_ = 
  let Position { octi, flashes } = checkFlashes Position { octi = octi_, row = 0, column = 0, flashes = Set.empty }
   in (Prelude.length flashes, octi)

checkFlashes :: Position -> Position 
checkFlashes = continue . checkFlash 
 where continue p = maybe p checkFlashes (next p)

checkFlash :: Position -> Position 
checkFlash pos 
 | hasFlashed pos  = pos
 | energy pos == 9 = checkAdjecent (flash pos)
 | otherwise       = inc pos

hasFlashed :: Position -> Bool
hasFlashed pos = member (row pos, column pos) (flashes pos)

checkAdjecent :: Position -> Position
checkAdjecent pos = Vec.foldr check pos (adjecent pos)
 where check (row, column) pos_ = checkFlash pos_ { row, column }

adjecent :: Position -> Vector (Int, Int)
adjecent pos = Vec.filter exists $ Vec.fromList
    [(row pos - 1, column pos)
    ,(row pos + 1, column pos)
    ,(row pos, column pos - 1)
    ,(row pos, column pos + 1)

    ,(row pos + 1, column pos + 1)
    ,(row pos + 1, column pos - 1)
    ,(row pos - 1, column pos + 1)
    ,(row pos - 1, column pos - 1)
    ]
 where 
  exists :: (Int, Int) -> Bool
  exists (r, c) = isJust $ (!? c) =<< (octi pos !? r)

energy :: Position -> Int
energy pos = (octi pos ! row pos) ! column pos

next :: Position -> Maybe Position
next pos
  | Vec.length currentRow == (column pos + 1) = nextRow
  | otherwise = nextColumn
 where
  currentRow = octi pos ! row pos
  nextColumn = Just pos { column = column pos + 1 }
  nextRow
    | Vec.length (octi pos) == (row pos + 1) = Nothing
    | otherwise = Just pos { row = row pos + 1, column = 0 }

set :: (Int -> Int) -> Position -> Position
set f pos = pos { octi = octi pos // [(row pos, (octi pos ! row pos) // [(column pos, f (energy pos))])] }

inc = set (+1)

flash = andFlash . set (const 0)
 where andFlash p = p { flashes = Set.insert (row p, column p) (flashes p) }
