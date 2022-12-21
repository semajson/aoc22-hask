module Day07 where
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Set        as Set

type Line = String
type LsOutput = [String]
type Path = String
type DirectoryName = String
type Directory = (DirectoryName, LsOutput)

main:: IO()
main = do
    input <- readFile "test_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    print parsed_data
    -- let sol = solve2 parsed_data
    -- print sol

parse :: [Line] -> [Directory]
parse output = parseOutput output ""

parseOutput :: [Line] -> Path -> [Directory]
parseOutput output currPath | isInfixOf "$ cd" currCommand = parseCd output currPath
                            | isInfixOf "$ ls" currCommand = parseLs output currPath
    where currCommand = head output

parseCd :: [Line] -> Path -> [Directory]
parseCd output currPath | isInfixOf "$ cd .." currCommand = parseCdOut output currPath
                        | isInfixOf "$ cd" currCommand = parseCdInto output currPath
                        | otherwise = error ("Unknown CD command" ++ currCommand)
    where currCommand = head output

parseCdOut :: [Line] -> Path -> [Directory]
parseCdOut output currPath = parseOutput remainingOutput newPath
   where remainingOutput = tail output
         newPath =  "/" ++ (intercalate "/" newPathSplit)
         newPathSplit = init pathSplit
         pathSplit =  filter ("" /= ) (splitOn "/" currPath) -- horrible hack to avoid leading ""


parseCdInto :: [Line] -> Path -> [Directory]
parseCdInto output currPath = parseOutput remainingOutput newPath
   where
         remainingOutput = tail output
         newPath = buildNewPath currPath currCommand
         currCommand = head output


buildNewPath :: Path -> Path -> Path
buildNewPath "" currCommand = dirName
    where dirName =  last (splitOn " " currCommand)
buildNewPath "/" currCommand = "/" ++ dirName
    where dirName =  last (splitOn " " currCommand)
buildNewPath currPath currCommand = currPath ++ "/" ++ dirName
    where dirName =  last (splitOn " " currCommand)


parseLs :: [Line] -> Path -> [Directory]
parseLs output currPath = case nextCommandIndex of
                        Just index -> concat [(parseOutput remainingOutput currPath), [newDirectory]]
                        Nothing    -> [newDirectory]
   where newDirectory = makeDirectory currPath lsOutput
         remainingOutput = case nextCommandIndex of
                               Just index -> drop index outputNoLsCommand
                               Nothing    -> []
         lsOutput = case nextCommandIndex of
                        Just index -> take index outputNoLsCommand
                        Nothing    -> outputNoLsCommand
         nextCommandIndex = findIndex isCommand outputNoLsCommand
         outputNoLsCommand = tail output

isCommand :: Line -> Bool
isCommand line = isInfixOf "$ " line

makeDirectory ::  Path -> [Line] -> Directory
makeDirectory currPath lsOutput = (currPath, lsOutput)
    where lsOutputDirsReplaced = map (replaceDirs currPath) lsOutput

replaceDirs ::  Path -> Line -> Line
replaceDirs "" currPath = error "invalid"
replaceDirs line currPath | isInfixOf "dir " line = currPath ++ "/" ++ (last (splitOn " " line))
                          | otherwise = line





-- solve1 :: Line -> Int
-- solve1 x = getFirstIndexOfPacket x 0
