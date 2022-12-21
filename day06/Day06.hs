-- module Day06 where
import           Data.List
import qualified Data.Set  as Set

type Line = String

parse :: [Line] -> Line
parse [a] = a


allUnique:: String -> Bool
allUnique x = length(x) == length(Set.fromList x)


getFirstIndexInPacket :: Line -> Int -> Int
getFirstIndexInPacket message index | allUnique(take 4 message) = index + 4
                                    | otherwise = getFirstIndexInPacket (tail message) (index + 1)

solve1 :: Line -> Int
solve1 x = getFirstIndexInPacket x 0


getFirstIndexOfessage :: Line -> Int -> Int
getFirstIndexOfessage message index | allUnique(take 14 message) = index + 14
                                    | otherwise = getFirstIndexOfessage (tail message) (index + 1)

solve2 :: Line -> Int
solve2 x = getFirstIndexOfessage x 0


main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve2 parsed_data
    print sol
