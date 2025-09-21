module Main where

import Debug.Trace

main :: IO ()
main = do
  puzzleData <- readFile "puzzle.txt"
  let puzzle = lines puzzleData
  let walkedPositions = solveP1 puzzle
  print $ length $ walkedPositions
  print $ solveP2 puzzle walkedPositions

printPuzzleLine :: [[Char]] -> [(Int, Int)] -> Int -> Int -> IO ()
printPuzzleLine puzzle walked width y = do
  mapM_ (\x -> do
      let walkedOnTail = (x, y) `elem` walked
      let charAt = (puzzle !! y) !! x
      putStr (if walkedOnTail then "X" else charAt : [])
    :: IO ()) [0 .. width - 1]
  putStrLn ""

solveP2 :: [[Char]] -> [(Int, Int)] -> Int
solveP2 puzzle walkedPositions =
   let startPosition = findStart puzzle 0
    in length $ filter (\position -> guardGetsStuck startPosition puzzle position) walkedPositions

guardGetsStuck :: (Int, Int) -> [[Char]] -> (Int, Int) -> Bool
guardGetsStuck (startX, startY) puzzle blockedPosition
  | blockedPosition == (startX, startY) = False
  | otherwise =
    let newPuzzle = placeWallAt blockedPosition puzzle
        solution = walkP2 newPuzzle [(startX, startY)] (startX, startY, 0)
     in traceShow solution solution

placeWallAt :: (Int, Int) -> [[Char]] -> [[Char]]
placeWallAt (x, y) puzzle =
  let beforeY = take y puzzle
      afterY = drop (y+1) puzzle
      beforeX = take x (puzzle !! y)
      afterX = drop (x+1) (puzzle !! y)
   in beforeY ++ [beforeX ++ ['#'] ++ afterX] ++ afterY

solveP1 :: [[Char]] -> [(Int, Int)]
solveP1 puzzle =
  let (x, y) = findStart puzzle 0
      walkedPositions = walkP1 puzzle [(x, y)] (x, y, 0)
      unique = uniqueAndValidPositions puzzle walkedPositions []
   in unique

uniqueAndValidPositions :: [[Char]] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
uniqueAndValidPositions _ [] output = output
uniqueAndValidPositions puzzle (first : remainder) output
  | not $ cordInsideMap puzzle first = uniqueAndValidPositions puzzle remainder output
  | first `elem` output = uniqueAndValidPositions puzzle remainder output
  | otherwise = uniqueAndValidPositions puzzle remainder (output ++ [first])

walkP1 :: [[Char]] -> [(Int, Int)] -> (Int, Int, Int) -> [(Int, Int)]
walkP1 puzzle walked (x, y, direction)
  | cordInsideMap puzzle (x, y) =
      let (nextX, nextY, newDirection) = nextTail puzzle (x, y, direction)
       in walkP1 puzzle ((nextX, nextY) : walked) $ (nextX, nextY, newDirection)
  | otherwise = walked

walkP2 :: [[Char]] -> [(Int, Int, Int)] -> (Int, Int, Int) -> Bool
walkP2 puzzle walked (x, y, direction)
  | repeatingOurselfs = True
  | cordInsideMap puzzle (x, y) =
        let (nextX, nextY, newDirection) = nextTail puzzle (x, y, direction)
        in walkP2 puzzle ((nextX, nextY) : walked) $ (nextX, nextY, newDirection)
  | otherwise = False
  where
    repeatingOurselfs = ((x, y) `elemCount` walked) > 3

elemCount :: Eq a => a -> [a] -> Int
elemCount _ [] = 0
elemCount n (first:remainder)
  | first == n = 1 + elemCount n remainder
  | otherwise = elemCount n remainder

nextTail :: [[Char]] -> (Int, Int, Int) -> (Int, Int, Int)
nextTail puzzle dirAndCord
  | not valid = (nextX, nextY, third dirAndCord) -- Out of map, move in the desired direction
  | ((puzzle !! nextY) !! nextX) == '#' = nextTail puzzle $ nextDirection dirAndCord -- Turn right
  | otherwise = (nextX, nextY, third dirAndCord) -- Move in the desired direction
  where
    (nextX, nextY) = nextInDirection dirAndCord
    valid = cordInsideMap puzzle (nextX, nextY)

third :: (a, b, c) -> c
third (_, _, c) = c

nextDirection :: (Int, Int, Int) -> (Int, Int, Int)
nextDirection (x, y, direction) = (x, y, (direction + 1) `mod` 4)

nextInDirection :: (Int, Int, Int) -> (Int, Int)
nextInDirection (x, y, direction) = case direction of
  0 -> (x, y - 1) -- up
  1 -> (x + 1, y) -- right
  2 -> (x, y + 1) -- down
  3 -> (x - 1, y)
  _ -> error ("invalid direction " ++ show direction)

cordInsideMap :: [[Char]] -> (Int, Int) -> Bool
cordInsideMap puzzle (x, y) =
  let height = length puzzle
      width = length $ head puzzle
   in y < height && y >= 0 && x < width && x >= 0

findStart :: [[Char]] -> Int -> (Int, Int)
findStart puzzle y
  | (length puzzle) == y = (-1, -1)
  | otherwise =
      let x = findStartOnLine (puzzle !! y) 0
       in if x == -1
            then findStart puzzle (y + 1)
            else (x, y)

findStartOnLine :: [Char] -> Int -> Int
findStartOnLine line x
  | (length line) == x = -1
  | (line !! x) == '^' = x
  | otherwise = findStartOnLine line (x + 1)
