-- module Day02 where
import           Data.List
import           Data.List.Split
import           Data.List.Split
import           Data.Maybe

type Line = String
type Shape = String
type Result = Int

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

shapeToIndex :: Shape -> Int
shapeToIndex "R" = 0
shapeToIndex "P" = 1
shapeToIndex "S" = 2
shapeToIndex x   = error ( "unexpected shape: " ++ x)

indexToShape :: Int -> Shape
indexToShape  0 = "R"
indexToShape  1 = "P"
indexToShape  2 = "S"
indexToShape _  = error "unexpected shape"

myShapeScore:: [Shape] -> Int
myShapeScore [_, myShape] = shapeScore (decodeShape myShape)
myShapeScore _            = error "unexpected"

resultMatrix = [[3, 6, 0],
                [0, 3, 6],
                [6, 0, 3]]

roundResult1 :: [Shape] -> Result
roundResult1 [encOppShape, encMyShape] = (resultMatrix !! (shapeToIndex oppShape)) !!(shapeToIndex myShape)
    where oppShape = decodeShape encOppShape
          myShape = decodeShape encMyShape
roundResult1 _ =  error "unexpected"

calcRoundScore1:: [Shape] -> Int
calcRoundScore1 round = (roundResult1 round) + (myShapeScore round)


solve1 :: [[Shape]] -> Int
solve1 stratergyGuide = sum (map calcRoundScore1 stratergyGuide)

decodeResult :: Shape -> Result
decodeResult "Y" = 3
decodeResult "X" = 0
decodeResult "Z" = 6
decodeResult _   = error "unexpected shape"


getMyShape2:: Result -> Shape -> Shape
getMyShape2 result oppShape = indexToShape(  fromJust (elemIndex result myShapeIndexOutcomes))
    where myShapeIndexOutcomes = resultMatrix!!(shapeToIndex oppShape)

calcRoundScore2:: [Shape] -> Int
calcRoundScore2 [encOppShape, encResult] = result + (shapeScore (getMyShape2 result oppShape))
    where result = decodeResult encResult
          oppShape = decodeShape encOppShape


solve2 :: [[Shape]] -> Int
solve2 stratergyGuide = sum (map calcRoundScore2 stratergyGuide)


main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve2 parsed_data
    print sol
