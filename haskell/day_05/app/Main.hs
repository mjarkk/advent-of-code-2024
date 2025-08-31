module Main where

import Debug.Trace

main :: IO ()
main = do
  puzzleData <- readFile "puzzle.txt"
  let puzzle = parse puzzleData
  print $ solve puzzle

solve :: ([(Int, Int)], [[Int]]) -> (Int, Int)
solve (orderings, updates) =
  let (valid, invalid) = groupUpdates ([], []) orderings updates
      validOrders = sum $ map takeMiddleNumber valid
      fixedOrders = sum $ map (`fix` orderings) invalid
   in (validOrders, fixedOrders)

fix :: [Int] -> [(Int, Int)] -> Int
fix numbers orderings =
  takeMiddleNumber $ fixR numbers orderings

fixR :: [Int] -> [(Int, Int)] -> [Int]
fixR [] _ = []
fixR (needle : remainder) orderings =
  let illegalNumbers = prevNumbers orderings needle
      replacements = filter (`elem` illegalNumbers) remainder
   in if null replacements
        then needle : fixR remainder orderings
        else
          -- swap the numbers
          let replacement = head replacements
              newNumbers = replacement : map (\n -> if n == replacement then needle else n) remainder
           in fixR newNumbers orderings

groupUpdates :: ([[Int]], [[Int]]) -> [(Int, Int)] -> [[Int]] -> ([[Int]], [[Int]])
groupUpdates results _ [] = results
groupUpdates (positive, negative) orderings updates =
  let firstUpdate = head updates
      nextUpdates = drop 1 updates
      valid = validUpdate orderings firstUpdate
      nextResults = if valid then (firstUpdate : positive, negative) else (positive, firstUpdate : negative)
   in groupUpdates nextResults orderings nextUpdates

takeMiddleNumber :: [Int] -> Int
takeMiddleNumber numbers = numbers !! ((length numbers) `div` 2)

validUpdate :: [(Int, Int)] -> [Int] -> Bool
validUpdate _ [] = True
validUpdate orderings update =
  let lastNum = last update
      before = init update
      illegalNumbers = nextNumbers orderings lastNum
      valid = not $ any (`elem` illegalNumbers) before
   in valid && validUpdate orderings before

nextNumbers :: [(Int, Int)] -> Int -> [Int]
nextNumbers orderings num = map snd $ filter (\set -> fst set == num) orderings

prevNumbers :: [(Int, Int)] -> Int -> [Int]
prevNumbers orderings num = map fst $ filter (\set -> snd set == num) orderings

-- Parse puzzle

parse :: String -> ([(Int, Int)], [[Int]])
parse input =
  let (orderings, updates) = sections input
   in (parseOrderings orderings, parseUpdates updates)

parseOrderings :: String -> [(Int, Int)]
parseOrderings input =
  let orderings = filter (/= "") $ lines input
   in map
        ( \ordering ->
            let splitIndex = indexOf "|" ordering
             in (read $ take splitIndex ordering, read $ drop (splitIndex + 1) ordering)
        )
        orderings

parseUpdates :: String -> [[Int]]
parseUpdates input =
  let updates = filter (/= "") $ lines input
   in map (`parseUpdate` []) updates

parseUpdate :: String -> [Int] -> [Int]
parseUpdate input update =
  let split = indexOf "," input
   in if split == -1
        then update ++ [read input]
        else parseUpdate (drop (split + 1) input) (update ++ [read $ take split input])

sections :: String -> (String, String)
sections input =
  let splitIndex = indexOf "\n\n" input
   in (take splitIndex input, drop (splitIndex + 2) input)

indexOf :: String -> String -> Int
indexOf needle haystack = indexOfR 0 needle haystack

indexOfR :: Int -> String -> String -> Int
indexOfR _ _ [] = -1
indexOfR offset needle haystack =
  if take (length needle) haystack == needle
    then offset
    else indexOfR (offset + 1) needle $ drop 1 haystack
