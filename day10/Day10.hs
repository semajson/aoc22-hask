-- module Day10 where
import           Data.List
import           Data.List.Split
import qualified Data.Set        as Set
import           Debug.Trace

type Line = String
type Command = (String, Maybe Int)
type SignalStrength = Int
type Cycle = Int
type RegisterVal = Int

main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve2 parsed_data
    mapM_ print sol

parse :: [Line] -> [Command]
parse lines = map (getCommand . (splitOn " ")) lines
    where
        getCommand:: [String] -> Command
        getCommand [a]    = (a, Nothing)
        getCommand [a, b] = (a, Just (toInt b))
        getCommand _      = error "Invalid getCommand input"

toInt :: String -> Int
toInt =  read

solve1 :: [Command] -> Int
solve1 commands = sum (map calcSignalStrength cyclesToCheckRegisterVals)
    where
        cyclesToCheckRegisterVals = traceShow (registerValsDuring) filter isCycleToCheck registerValsDuring
        isCycleToCheck :: (Cycle, RegisterVal) -> Bool
        isCycleToCheck (cycle, _) = elem cycle cyclesToCheck
        cyclesToCheck = [20, 60, 100, 140, 180, 220]
        -- bit of a hack, "during" means the value at the middle of the cycle, not the end -
        -- so need to shift by one
        registerValsDuring = [(1, 0)] ++ (map duringCycle registerVals)
        duringCycle (cycle, registerVal) = (cycle +1, registerVal)
        registerVals = calcRegisterVals commands 0 220 firstRegisterVal
        firstRegisterVal = 1

calcSignalStrength :: (Cycle, RegisterVal) -> Int
calcSignalStrength (cycle, registerVal) = cycle * registerVal

calcRegisterVals :: [Command] -> Cycle -> Int -> RegisterVal -> [(Cycle, RegisterVal)]
calcRegisterVals [] currCycle maxCycle registerVal = []
calcRegisterVals commands currCycle maxCycle registerVal
    | currCycle > maxCycle = []
    | otherwise = commandOutputs ++ (calcRegisterVals remainingCommands newCycle maxCycle newRegisterVal)
    where
        (newCycle, newRegisterVal) = last commandOutputs
        commandOutputs = runCommand thisCommand currCycle registerVal
        remainingCommands = tail commands
        thisCommand = head commands

runCommand :: Command -> Cycle -> RegisterVal -> [(Cycle, RegisterVal)]
runCommand ("noop", Nothing) curCycle registerVal = [(nextCycle, registerVal)]
    where nextCycle = curCycle + 1
runCommand ("addx", Just value) curCycle registerVal = [(nextCycle, registerVal),
                                                        (nextNextCycle, newRegisterVal)]
    where
        nextNextCycle = nextCycle + 1
        nextCycle = curCycle + 1
        newRegisterVal = registerVal + value

solve2 :: [Command] -> [String]
solve2 commands = rows
    where
        rows = map concat rowsList
        rowsList = traceShow (registerValsDuring) splitEvery 40 pixelList
        pixelList = map getPixelVal registerValsDuring
        registerValsDuring = [(1, 0)] ++ (map duringCycle registerVals)
        duringCycle (cycle, registerVal) = (cycle +1, registerVal)
        registerVals = calcRegisterVals commands 0 240 firstRegisterVal
        firstRegisterVal = 1

getPixelVal :: (Cycle, RegisterVal) -> String
getPixelVal (cycle, registerVal)
    | diff < 2 = "#"
    | otherwise = "."
    where
        diff =  abs (xValue - spritePos)
        spritePos = registerVal +1
        xValue = cycle `mod` 40
