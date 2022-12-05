import           Data.List
import           Data.List.Split

type Line = String
type Snack = Int

toInt :: [String] -> [Int]
toInt = map read

parse :: [Line] -> [[Snack]]
parse x = map toInt (splitWhen (== "") x)

solve1 :: [[Snack]] -> Int
solve1 elfSnacks = maximum (totalCals)
    where totalCals = map sum elfSnacks

solve2 :: [[Snack]] -> Int
solve2 elfSnacks = sum (take 3 orderedTotalCals)
    where totalCals = map sum elfSnacks
          orderedTotalCals = reverse (sort totalCals)

main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve2 parsed_data
    print sol
