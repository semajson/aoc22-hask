-- module Day02 where
import           Data.List
import           Data.List.Split
import           Data.List.Split

type Line = String
type Shape = String

parse :: [Line] -> [[Shape]]
parse x = map (splitOn " ") x

decodeShape :: Shape -> Shape
decodeShape "X" = "R"
decodeShape "A" = "R"
decodeShape "Y" = "P"
decodeShape "B" = "P"
decodeShape "C" = "S"
decodeShape "Z" = "S"
decodeShape _   = error "unexpected shape"

shapeScore :: Shape -> Int
shapeScore "R" = 1
shapeScore "P" = 2
shapeScore "S" = 3
shapeScore _   = error "unexpected shape"

shapeIndex :: Shape -> Int
shapeIndex "R" = 0
shapeIndex "P" = 1
shapeIndex "S" = 2
shapeIndex _   = error "unexpected shape"

myShapeScore:: [Shape] -> Int
myShapeScore [_, myShape] = shapeScore (decodeShape myShape)
myShapeScore _            = error "unexpected"

resultMatrix = [[3, 6, 0],
                [0, 3, 6],
                [6, 0, 3]]

roundResult :: [Shape] -> Int
roundResult [encOppShape, encMyShape] = (resultMatrix !! (shapeIndex oppShape)) !!(shapeIndex myShape)
    where oppShape = decodeShape encOppShape
          myShape = decodeShape encMyShape
roundResult _ =  error "unexpected"

calcRoundScore:: [Shape] -> Int
calcRoundScore round = (roundResult round) + (myShapeScore round)

solve1 :: [[Shape]] -> Int
solve1 stratergyGuide = sum (map calcRoundScore stratergyGuide)

main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve1 parsed_data
    print sol
