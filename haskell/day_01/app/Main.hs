module Main where

import Data.List (sort)
import Data.Text qualified as T

main :: IO ()
main = do
  contents <- readFile "puzzle.txt"
  let results = parseInput contents
  print results

parseInput :: String -> (Int, Int)
parseInput input =
  let contentLines = filter (not . null) (lines input)
      lineNumbers = map parseLine contentLines
      sets = unzip lineNumbers
      sortedSets = (sort (fst sets), sort (snd sets))
      lineNumbers2 = uncurry zip sortedSets
      diffs = map (\(a, b) -> abs (a - b)) lineNumbers2
      counts = map (\x -> totalOf (snd sets) x) (fst sets)
   in (sum diffs, sum counts)

parseLine :: String -> (Int, Int)
parseLine line =
  let splits = T.splitOn (T.pack "   ") (T.pack line)
      numberString = map (\part -> T.unpack part) splits
      numbers = map read numberString
   in case numbers of
        [x, y] -> (x, y)
        _ -> error "Line does not contain 2 numbers"

totalOf :: [Int] -> Int -> Int
totalOf values x =
  sum (filter (== x) values)
