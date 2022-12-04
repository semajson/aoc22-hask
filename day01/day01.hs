import Data.List
import Data.List.Split
import System.IO


toInt :: [String] -> [Int]
toInt = map read

parse :: [String] -> [[Int]]
parse x = map toInt (splitWhen (== "") x)

solve :: [[Int]] -> Int
solve test = maximum totalCals
    where totalCals = map sum test


main = do
    input <- readFile "test_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve parsed_data
    print sol