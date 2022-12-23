-- module Day08 where
import           Data.List
import qualified Data.Set  as Set

type Line = String
type SeenCount = Int
type Height = Int
type Tree = (Height, SeenCount)
type Grid = [[Tree]]
type Row = [Tree]

main:: IO()
main = do
    input <- readFile "real_inputs.txt"
    let input_lines =lines input
    let parsed_data = parse input_lines
    let sol = solve1 parsed_data
    print sol
    -- print " "
    -- print " "
    -- mapM_ print (debugSolve1 parsed_data)

-- todo, rewrite with list comprehension
parse :: [Line] -> Grid
parse input = map ((map convert)) input
    where
        convert :: Char -> (Int, SeenCount)
        convert x = ((toInt x), 0)

toInt :: Char -> Int
toInt =  read . pure

solve1 :: Grid -> Int
solve1 grid =  (sum . (map (length . (filter (0<))))) gridSeenCount
    where gridSeenCount = map (map snd) markedGrid
          markedGrid = applyAllDirections grid markSeenTrees

-- debug only
debugSolve1 :: Grid -> Grid
debugSolve1 grid =  rotateGridLeft markedGrid
    where gridSeenCount = map (map snd) markedGrid
          markedGrid = applyAllDirections grid markSeenTrees

applyAllDirections :: Grid -> (Grid -> Grid) -> Grid
applyAllDirections grid func = (func . rotateGridLeft . func . rotateGridLeft . func . rotateGridLeft . func) grid

markSeenTrees :: Grid -> Grid
markSeenTrees grid =  map (markSeenTreesRow (-1)) grid

markSeenTreesRow:: Int -> Row -> Row
markSeenTreesRow highestSeenTree []= []
markSeenTreesRow highestSeenTree row
    -- | thisTreeHeight == 0 = [incrementSeenCount thisTree] ++ (markSeenTreesRow thisTreeHeight remainingTrees)
    | thisTreeHeight > highestSeenTree = [incrementSeenCount thisTree] ++ (markSeenTreesRow thisTreeHeight remainingTrees)
    | otherwise = [thisTree] ++ (markSeenTreesRow highestSeenTree remainingTrees)
    where remainingTrees = tail row
          thisTreeHeight = fst thisTree
          thisTree = head row
          incrementSeenCount :: Tree -> Tree
          incrementSeenCount (height, seenCount) = (height, seenCount + 1)

rotateGridLeft :: Grid -> Grid
rotateGridLeft grid = (reverse . transpose) grid

