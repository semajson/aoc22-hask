import Data.List
import System.IO

-- parse:: String -> [String]
-- parse =

main = do
    input <- readFile "test_inputs.txt"
    let input_lines =lines input
    print input_lines