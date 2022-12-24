-- module Day09 where
import           Data.List
import           Data.List.Split
import qualified Data.Set        as Set

type Line = String
type Direction = String
type Step = Int
type Command = (Direction, Step)
-- type Command = [String]

main:: IO()
main = do
    input <- readFile "test_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    -- let sol = solve1 parsed_data
    print parsed_data


parse :: [Line] -> [Command]
parse lines = map (getCommand . (splitOn " ")) lines
    where getCommand:: [String] -> Command
          getCommand [a,b] = (a, (toInt b))
          getCommand x     = error "Invalid getCommand input"

toInt :: String -> Int
toInt =  read

-- solve1 :: Line -> Int
-- solve1 x = getFirstIndexOfPacket x 0


-- solve2 :: Line -> Int
-- solve2 x = getFirstIndexOfMessage x 0


