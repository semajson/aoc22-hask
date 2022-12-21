-- module Day07 where
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
    let sol = solve1 parsed_data
    print sol

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
makeDirectory currPath lsOutput = (currPath, lsOutputDirsReplaced)
    where lsOutputDirsReplaced = map (replaceDirs currPath) lsOutput



replaceDirs ::  Path -> Line -> Line
replaceDirs currPath "" = error "invalid"
replaceDirs currPath line | isInfixOf "dir " line = buildNewPath currPath line
                          | otherwise = line



solve1 :: [Directory] -> Int
solve1 directories =  sum ( filter (100000>) directoriesSizes)
    where directoriesSizes = map (calcSizeDir directories) directories


calcSizeDir :: [Directory] ->  Directory -> Int
calcSizeDir directoryTable directory = sum (map (calcSizeLsLine directoryTable)  (snd directory))

calcSizeLsLine :: [Directory] -> Line -> Int
calcSizeLsLine directoryTable line  | isPrefixOf  "/" line = calcSizeDir directoryTable (getDirectory directoryTable line)
                                    | otherwise = toInt ( head (splitOn " " line))

toInt :: String -> Int
toInt =  read

getDirectory :: [Directory] -> String -> Directory
getDirectory directoryTable line =  case matchingDir of
                        Just foundDir -> foundDir
                        Nothing       -> error "Didn't find match"
    where matchingDir = find ((line==) . fst) directoryTable
