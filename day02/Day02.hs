module Day02 (roundResult) where
import           Data.List
import           Data.List.Split
import           Data.List.Split

type Line = String
type Shape = String

-- toInt :: [String] -> [Int]
-- toInt = map read

parse :: [Line] -> [[Shape]]
parse x = map (splitOn " ") x

shapeScore :: Shape -> Int
-- Rock
shapeScore "X" = 1
shapeScore "A" = 1

-- Paper
shapeScore "Y" = 2
shapeScore "B" = 2

-- Scissors
shapeScore "C" = 3
shapeScore "Z" = 3

shapeScore _   = error "unexpected shape"

myShapeScore:: [Shape] -> Int
myShapeScore [_, myShape] = shapeScore myShape
myShapeScore _            = error "unexpected"

shapeIndex :: Shape -> Int
shapeIndex shape = (shapeScore shape) - 1

resultMatrix = [[3, 6, 0],
                [0, 3, 6],
                [6, 0, 3]]


roundResult :: [Shape] -> Int
roundResult [oppShape, myShape] = (resultMatrix !! (shapeIndex oppShape)) !!(shapeIndex myShape)

calcRoundScore:: [Shape] -> Int
calcRoundScore round = (roundResult round) + (myShapeScore round)

solve1 :: [[Shape]] -> Int
solve1 stratergyGuide = sum (map calcRoundScore stratergyGuide)

main:: IO()
main = do
    input <- readFile "test_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve1 parsed_data
    print sol
