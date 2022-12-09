-- module Day05 where
import           Data.List
import           Data.List.Split
import           Data.Text       (pack, replace, unpack)

type Line = String
type Stack = String
type Command = (Int, Int, Int)

toInt :: [String] -> [Int]
toInt = map read

tuplify3 :: [a] -> (a,a,a)
tuplify3 [x,y,z] = (x,y,z)
tuplify3 _       = error "can't convert to tuple 3"

-- repalce substr a withi substr b in string c
-- have to go via text, looks like this isn't native in haskell :(
stringReplace:: String -> String -> String -> String
stringReplace a b c =  unpack  ((replace (pack a) (pack b)) (pack c))

parseStacks :: [Line] -> [Stack]
parseStacks rawStacks = map concat stacks
    where
        stacks = transpose cratesByLevel
        cratesByLevel = map (map ((stringReplace " " "") . (stringReplace "[" "") . (stringReplace "]" ""))) splitUp
        splitUp = map (chunksOf 4) noLabels
        noLabels = filter (isInfixOf "[") rawStacks


parseCommands :: [Line] -> [Command]
parseCommands rawCommands = map tuplify3 intCommands
    where intCommands = map toInt stringCommands
          stringCommands =  map (splitOn " ") trimmedCommands
          trimmedCommands = map ((stringReplace "move " "") . (stringReplace " from" "") . (stringReplace " to" "")) rawCommands


parse :: [Line] -> ([Stack], [Command])
parse inputLines  = (parseStacks rawStacks, parseCommands rawCommands)
    where rawStacks = stacksAndCommands !! 0
          rawCommands = stacksAndCommands !! 1
          stacksAndCommands = splitWhen (== "") inputLines


getStackIndex :: Int -> Int
getStackIndex num = num -1

replaceCommand :: Int -> String -> [String] -> [String]
replaceCommand index command commands = before ++ [command] ++ after
  where
    (before, _:after) = splitAt index commands


doCommand :: Command -> [Stack] -> [Stack]
doCommand (moveNum, from, to) stacks =  (replaceCommand toIndex newTo (replaceCommand fromIndex newFrom stacks))
    where
        newFrom = drop moveNum (stacks !! fromIndex)
        newTo = cratesToMove ++ (stacks !! toIndex)
        cratesToMove = reverse (take moveNum (stacks !! fromIndex))
        fromIndex = getStackIndex from
        toIndex = getStackIndex to

doAllCommands :: [Stack] -> [Command] -> [Stack]
doAllCommands stacks [(a,b,c)] = doCommand (a,b,c) stacks
doAllCommands stacks (command:remaining) = doAllCommands (doCommand command stacks) remaining

getTops :: [Stack] -> String
getTops stacks = (transpose stacks) !! 0

solve1 :: ([Stack], [Command])-> String
solve1 (stacks, commands) = getTops (doAllCommands stacks commands)


doNewCommand :: Command -> [Stack] -> [Stack]
doNewCommand (moveNum, from, to) stacks =  (replaceCommand toIndex newTo (replaceCommand fromIndex newFrom stacks))
    where
        newFrom = drop moveNum (stacks !! fromIndex)
        newTo = cratesToMove ++ (stacks !! toIndex)
        cratesToMove = take moveNum (stacks !! fromIndex)
        fromIndex = getStackIndex from
        toIndex = getStackIndex to

doAllNewCommands :: [Stack] -> [Command] -> [Stack]
doAllNewCommands stacks [(a,b,c)] = doNewCommand (a,b,c) stacks
doAllNewCommands stacks (command:remaining) = doAllNewCommands (doNewCommand command stacks) remaining

solve2 :: ([Stack], [Command])-> String
solve2 (stacks, commands) = getTops (doAllNewCommands stacks commands)


main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    print "test"
    print parsed_data
    let sol = solve2 parsed_data
    print sol
