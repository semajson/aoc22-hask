-- module Day04 where
import           Data.List.Split

type Line = String
type Pair = [[Int]]

toInt :: [String] -> [Int]
toInt = map read

parse :: [Line] -> [Pair]
parse allLines = intAssignements
   where intAssignements = map (map toInt) stringAssignements
         stringAssignements = map (map (splitOn "-")) assignementsRaw
         assignementsRaw = map (splitOn ",") allLines


pairFullyOverlap :: Pair -> Bool
pairFullyOverlap [[a,b], [c,d]] | (a <= c) && (d <= b) = True
                                | (c <= a) && (b <= d) = True
                                | otherwise = False
pairFullyOverlap _              = error "unexpected input"

convertBoolToCount :: Bool -> Int
convertBoolToCount False = 0
convertBoolToCount True  = 1

solve1 :: [Pair] -> Int
solve1 pairs = sum (map ( convertBoolToCount . pairFullyOverlap) pairs)

pairPartlyOverlap :: Pair -> Bool
pairPartlyOverlap [[a,b], [c,d]] | (a <= c) && (c <= b) = True
                                 | (a <= d) && (d <= b) = True
                                 | (c <= a) && (b <= d) = True
                                 | otherwise = False
pairPartlyOverlap _              = error "unexpected input"

solve2 :: [Pair] -> Int
solve2 pairs = sum (map ( convertBoolToCount . pairPartlyOverlap) pairs)

main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    print parsed_data
    let sol = solve2 parsed_data
    print sol
