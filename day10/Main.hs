import Data.List

main = do
  system <- parse <$> getContents
  print (part1 system)
  print (part2 system)

data Parser = Parser String [(Char, Char)]
type Chunk = [Char]
data Error = Corrupt Char Char | Incomplete [(Char, Char)] deriving (Show)

parse :: String -> [Either Error Chunk]
parse = map (\line -> run (Parser line [])) . lines

run :: Parser -> Either Error Chunk
run (Parser (open:xs) toClose) = 
  case pair open of
    Just close -> run $ Parser xs ((open, close) : toClose)
    Nothing    -> 
      let ((o, c):closes) = toClose
      in if c == open
           then (o:) <$> run (Parser xs closes)
           else Left (Corrupt o open)
run (Parser [] []) = Right []
run (Parser [] xs) = Left (Incomplete xs)

pair :: Char -> Maybe Char
pair '(' = Just ')'
pair '{' = Just '}'
pair '<' = Just '>'
pair '[' = Just ']'
pair _   = Nothing

part1 :: [Either Error Chunk] -> Int
part1 = sum . map score
 where
  score (Left (Corrupt _ close)) = scoreOf close
  score _ = 0

  scoreOf :: Char -> Int
  scoreOf ')' = 3
  scoreOf ']' = 57
  scoreOf '}' = 1197
  scoreOf '>' = 25137

part2 :: [Either Error Chunk] -> Int
part2 = present . filter (/=0) . map completion
 where
  present = (\list -> list !! div (length list) 2) . sort 
  
  completion (Left (Incomplete remaining)) = complete remaining
  completion _ = 0
  complete = foldl (\acc m -> (acc * 5) + scoreOf (snd m)) 0

  scoreOf ')' = 1
  scoreOf ']' = 2
  scoreOf '}' = 3
  scoreOf '>' = 4
