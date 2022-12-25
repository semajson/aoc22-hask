-- module Day09 where
import           Data.List
import           Data.List.Split
import qualified Data.Set        as Set

type Line = String
type Direction = String
type Step = Int
type Command = (Direction, Step)
type Position = (Int, Int)

main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve1 parsed_data
    print sol


parse :: [Line] -> [Command]
parse lines = map (getCommand . (splitOn " ")) lines
    where getCommand:: [String] -> Command
          getCommand [a,b] = (a, (toInt b))
          getCommand x     = error "Invalid getCommand input"

toInt :: String -> Int
toInt =  read

solve1 :: [Command] -> Int
solve1 commands = (length . nub) (getTailVisistedSquares (0,0) (0,0) directions)
    where directions = convertCommandsToDirections commands

convertCommandsToDirections :: [Command] -> [Direction]
convertCommandsToDirections [] = []
convertCommandsToDirections commands = directions ++ (convertCommandsToDirections remainingCommands)
    where remainingCommands = tail commands
          directions = replicate times direction
          (direction, times) = head commands

getTailVisistedSquares:: Position -> Position -> [Direction] -> [Position]
getTailVisistedSquares headPos tailPos [] = []
getTailVisistedSquares headPos tailPos directions = [newTailPos] ++ (getTailVisistedSquares newHeadPos newTailPos remainingDirections)
    where (newHeadPos, newTailPos) = moveDirection curDirection headPos tailPos
          remainingDirections = tail directions
          curDirection= head directions

moveDirection :: String -> Position -> Position -> (Position, Position)
moveDirection direction headPos tailPos = (newHeadPos, newTailPos)
    where newTailPos = calcNewTailPos newHeadPos tailPos
          newHeadPos = calcNewHeadPos direction headPos

calcNewHeadPos :: String -> Position -> Position
calcNewHeadPos "U" headPos = newHeadPos
    where newHeadPos = (headPosX, headPosY + 1)
          (headPosX, headPosY) = headPos
calcNewHeadPos "D" headPos = newHeadPos
    where newHeadPos = (headPosX, headPosY - 1)
          (headPosX, headPosY) = headPos
calcNewHeadPos "L" headPos = newHeadPos
    where newHeadPos = (headPosX -1, headPosY)
          (headPosX, headPosY) = headPos
calcNewHeadPos "R" headPos = newHeadPos
    where newHeadPos = (headPosX +1, headPosY)
          (headPosX, headPosY) = headPos

calcNewTailPos :: Position -> Position -> Position
calcNewTailPos headPos tailPos
    | (headPosX == tailPosX ) && ((headPosY - tailPosY) == 2) = (tailPosX, tailPosY + 1)
    | (headPosX == tailPosX ) && ((headPosY - tailPosY) == -2) = (tailPosX, tailPosY - 1)
    | ((headPosX - tailPosX ) == 2 ) && (headPosY == tailPosY) = (tailPosX + 1, tailPosY)
    | ((headPosX - tailPosX ) == -2) && (headPosY == tailPosY) = (tailPosX - 1, tailPosY)

    | ((headPosX - tailPosX ) == 1) && ((headPosY - tailPosY) == 2)  = (tailPosX +1, tailPosY +1)
    | ((headPosX - tailPosX ) == 2) && ((headPosY - tailPosY) == 1)  = (tailPosX +1, tailPosY +1)

    | ((headPosX - tailPosX ) == 1) && ((headPosY - tailPosY) == -2)  = (tailPosX +1, tailPosY -1)
    | ((headPosX - tailPosX ) == 2) && ((headPosY - tailPosY) == -1)  = (tailPosX +1, tailPosY -1)

    | ((headPosX - tailPosX ) == -1) && ((headPosY - tailPosY) == 2)  = (tailPosX -1, tailPosY +1)
    | ((headPosX - tailPosX ) == -2) && ((headPosY - tailPosY) == 1)  = (tailPosX -1, tailPosY +1)

    | ((headPosX - tailPosX ) == -1) && ((headPosY - tailPosY) == -2)  = (tailPosX -1, tailPosY -1)
    | ((headPosX - tailPosX ) == -2) && ((headPosY - tailPosY) == -1)  = (tailPosX -1, tailPosY -1)
    | otherwise = tailPos

    where (tailPosX, tailPosY) = tailPos
          (headPosX, headPosY) = headPos

-- solve2 :: Line -> Int
-- solve2 x = getFirstIndexOfMessage x 0


