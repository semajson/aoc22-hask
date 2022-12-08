-- module Day04 where
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.List.Split
import           Data.Maybe

type Line = String
type Pair = [[Int]]

toInt :: [String] -> [Int]
toInt = map read

parse :: [Line] -> [Pair]
parse allLines = intAssignements
   where intAssignements = map (map toInt) stringAssignements
         stringAssignements = map (map (splitOn "-")) assignementsRaw
         assignementsRaw = map (splitOn ",") allLines


pairFullyContained :: Pair -> Bool
pairFullyContained [[a,b], [c,d]] | (a <= c) && (d <= b) = True
                                  | (c <= a) && (b <= d) = True
                                  | otherwise = False
pairFullyContained _              = error "unexpected input"

convertBoolToCount :: Bool -> Int
convertBoolToCount False = 0
convertBoolToCount True  = 1

solve1 :: [Pair] -> Int
solve1 pairs = sum (map ( convertBoolToCount . pairFullyContained) pairs)


-- solve2 :: [Pair] -> Int
-- solve2 rucksackGroup= sum (map (lookupPriority . findCommonItem) rucksackGroup)

main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    print parsed_data
    let sol = solve1 parsed_data
    print sol
