module Main where

main :: IO ()
main = do
  puzzleData <- readFile "puzzle.txt"
  let parsed = parse puzzleData
  let size = (length parsed, length $ head parsed)
  print $ findAllY parsed size + findAllX parsed size + findDiagonal1 parsed size
  print $ findAllXmases parsed size

findAllY :: [[Char]] -> (Int, Int) -> Int
findAllY area (height, width) =
  sum $ map (\y -> sum $ map (\x -> isXmas [getCord area (x + i) y | i <- [0 .. 3]]) [0 .. width - 4]) [0 .. height - 1]

findAllX :: [[Char]] -> (Int, Int) -> Int
findAllX area (height, width) =
  sum $ map (\x -> sum $ map (\y -> isXmas [getCord area x (y + i) | i <- [0 .. 3]]) [0 .. height - 4]) [0 .. width - 1]

findDiagonal1 :: [[Char]] -> (Int, Int) -> Int
findDiagonal1 area (height, width) =
  let yr = [0 .. height - 4]
      xr = [0 .. width - 4]
   in sum $ map (\y -> sum $ map (\x -> hasDiagonal area x y) xr) yr

hasDiagonal :: [[Char]] -> Int -> Int -> Int
hasDiagonal area x y =
  isXmas [getCord area (x + i) (y + i) | i <- [0 .. 3] :: [Int]]
    + isXmas [getCord area (x + i) (3 - i + y) | i <- [0 .. 3] :: [Int]]

parse :: String -> [[Char]]
parse puzzleData = filter (/= "") $ lines puzzleData

getCord :: [[Char]] -> Int -> Int -> Char
getCord area x y = (area !! y) !! x

isXmas :: String -> Int
isXmas "XMAS" = 1
isXmas "SAMX" = 1
isXmas _ = 0

findAllXmases :: [[Char]] -> (Int, Int) -> Int
findAllXmases area (height, width) =
  let yr = [0 .. height - 3]
      xr = [0 .. width - 3]
   in sum $ map (\y -> sum $ map (\x -> if hasCross area x y then 1 else 0) xr) yr

hasCross :: [[Char]] -> Int -> Int -> Bool
hasCross area x y =
  let center = getCord area (x + 1) (y + 1)
      topLeft = getCord area x y
      topRight = getCord area (x + 2) y
      bottomLeft = getCord area x (y + 2)
      bottomRight = getCord area (x + 2) (y + 2)
   in center == 'A' && isMas topLeft bottomRight && isMas topRight bottomLeft

isMas :: Char -> Char -> Bool
isMas 'M' 'S' = True
isMas 'S' 'M' = True
isMas _ _ = False
