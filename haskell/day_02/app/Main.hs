module Main where

-- import Debug.Trace

main :: IO ()
main = do
  puzzle <- parseInput <$> readFile "puzzle.txt"
  print $ solveAll puzzle

parseInput :: String -> [[Int]]
parseInput input =
  map parseLine $ filter (/= "") $ lines input

parseLine :: String -> [Int]
parseLine line =
  map read (words line)

solveAll :: [[Int]] -> (Int, Int)
solveAll input =
  let solutionP1 = sum $ map (\singlePuzzle -> isSafe singlePuzzle False) input
      solutionP2 = sum $ map (\singlePuzzle -> isSafe singlePuzzle True) input
   in (solutionP1, solutionP2)

isSafe :: [Int] -> Bool -> Int
isSafe input mightHaveFaulty =
  let result = isSafeR input Unknown
   in case (result, mightHaveFaulty) of
        (True, _) -> 1
        (False, True) -> fromEnum $ any (\i -> isSafeR (removeIndex input i) Unknown) [0 .. (length input) - 1]
        (_) -> 0

removeIndex :: [Int] -> Int -> [Int]
removeIndex list index = (take index list) ++ (drop (index + 1) list)

data Direction = Unknown | Up | Down deriving (Eq, Show)

isSafeR :: [Int] -> Direction -> Bool
isSafeR (first : second : remainder) direction =
  let diff = first - second
      diffAbs = abs diff
      (newDirection, directionValidValue) = directionValid diff direction
      diffValid = diffAbs > 0 && diffAbs <= 3
   in directionValidValue && diffValid && isSafeR (second : remainder) newDirection
isSafeR (_) _ = True -- If there are no more than 2 elements left in the list always return true

directionValid :: Int -> Direction -> (Direction, Bool)
directionValid diff Unknown = (diffDirection diff, True)
directionValid diff direction = (direction, (diffDirection diff) == direction)

diffDirection :: Int -> Direction
diffDirection diff = case diff > 0 of
  True -> Up
  False -> Down
