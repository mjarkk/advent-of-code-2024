module Main where

-- import Debug.Trace
import Data.List (elemIndex)
import Text.Read (readMaybe)

main :: IO ()
main = do
  puzzle <- readFile "puzzle.txt"
  print $ solve puzzle

solve :: String -> (Int, Int)
solve input = solveR input True (0, 0)

solveR :: String -> Bool -> (Int, Int) -> (Int, Int)
solveR "" _ results = results
solveR input enabled results = case () of
  _
    | take 7 input == "don't()" -> solveR (drop 7 input) False results
    | take 4 input == "do()" -> solveR (drop 4 input) True results
    | take 4 input == "mul(" ->
        let (value, offset) = isMul $ drop 4 input
            newResults = (fst results + value, snd results + if enabled then value else 0)
         in solveR (drop (4 + offset) input) enabled newResults
  _ -> solveR (drop 1 input) enabled results

isMul :: [Char] -> (Int, Int)
isMul input =
  let commaIndex = elemIndex ',' input
      closingParamIndex = elemIndex ')' input
   in case (commaIndex, closingParamIndex) of
        (Just i, Just j)
          | j > i ->
              let firstNum = readMaybe $ take i input :: Maybe Int
                  secondNum = readMaybe $ drop (i + 1) $ take j input :: Maybe Int
               in case (firstNum, secondNum) of
                    (Just a, Just b) -> (a * b, j + 1)
                    _ -> (0, 0)
        _ -> (0, 0)
