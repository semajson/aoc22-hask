-- module Day08 where
import           Data.List
import qualified Data.Set  as Set

type Line = String
type SeeCount = Int
-- type Grid = [[(Int, SeeCount)]]
type Grid = [[Int]]

main:: IO()
main = do
    input <- readFile "test_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    print parsed_data
    -- let sol = solve2 parsed_data
    -- print sol

-- todo, rewrite with list comprehension
parse :: [Line] -> Grid
parse input = map (map toInt) input

toInt :: Char -> Int
toInt =  read . pure

-- toInt :: [String] -> [Int]
-- toInt = map read

-- solve1 :: Line -> Int
-- solve1 x = getFirstIndexOfPacket x 0
