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


moveBlocks1by1 :: Stack -> Stack
moveBlocks1by1 stack = reverse stack

doCommand :: Command -> [Stack] -> (Stack -> Stack) -> [Stack]
doCommand (moveNum, from, to) stacks moveBlocks =  (replaceCommand toIndex newTo (replaceCommand fromIndex newFrom stacks))
    where
        newFrom = drop moveNum (stacks !! fromIndex)
        newTo = cratesToMove ++ (stacks !! toIndex)
        cratesToMove = moveBlocks (take moveNum (stacks !! fromIndex))
        fromIndex = getStackIndex from
        toIndex = getStackIndex to


-- Recursive function, applies all the commands on the stack recursively
doAllCommands :: [Stack] -> [Command] -> (Stack -> Stack) -> [Stack]
doAllCommands stacks [(a,b,c)] moveBlocks = doCommand (a,b,c) stacks moveBlocks
doAllCommands stacks (command:remaining) moveBlocks = doAllCommands (doCommand command stacks moveBlocks) remaining moveBlocks

getTops :: [Stack] -> String
getTops stacks = (transpose stacks) !! 0

solve1 :: ([Stack], [Command])-> String
solve1 (stacks, commands) = getTops (doAllCommands stacks commands moveBlocks1by1)

moveBlocksAllAtOnce :: Stack -> Stack
moveBlocksAllAtOnce stack =  stack

solve2 :: ([Stack], [Command])-> String
solve2 (stacks, commands) = getTops (doAllCommands stacks commands moveBlocksAllAtOnce)


main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve2 parsed_data
    print sol
