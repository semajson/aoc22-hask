-- module Day03 where
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.List.Split
import           Data.Maybe

type Line = String
type Rucksack = [String]

pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]

splitInHalf :: String -> [String]
splitInHalf xs = pairToList(splitAt ((length xs + 1) `div` 2) xs)

parse :: [Line] -> [Rucksack]
parse x = map splitInHalf x

priorityVals = ['a'..'z'] ++ ['A'.. 'Z']
lookupPriority :: Char -> Int
lookupPriority letter =  fromJust (elemIndex letter priorityVals) + 1

getErrorLetters :: Rucksack -> [Char]
getErrorLetters rucksack = (intersect (rucksack!!0) (rucksack!!1))

-- only care about the single error letter in each rucksack
errorPriority :: Rucksack -> Int
errorPriority rucksack = lookupPriority firstErrorLetter
    where firstErrorLetter = errorLetters !!0
          errorLetters = getErrorLetters rucksack

solve1 :: [Rucksack] -> Int
solve1 rucksacks = sum (map errorPriority rucksacks)


type RucksackWhole = String
type RucksackGroup = [RucksackWhole]

parse2 :: [Line] -> [RucksackGroup]
parse2 rucksacks = chunksOf 3 rucksacks

-- assumes intersectingItems will only be of length 1
findCommonItem ::  [RucksackWhole] -> Char
findCommonItem rucksackGroup = (intersectingItems !! 0)
    where intersectingItems = intersect (rucksackGroup !! 0) (intersect (rucksackGroup !! 1) (rucksackGroup !!2 ))


solve2 :: [RucksackGroup] -> Int
solve2 rucksackGroup= sum (map (lookupPriority . findCommonItem) rucksackGroup)

main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines

    let parsed_data2 = parse2 input_lines
    let sol = solve2 parsed_data2
    print sol
