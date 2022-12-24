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
    let input_lines = lines input
    let parsed_data1 = parse input_lines
    let parsed_data2 = parse2 input_lines

    -- print " "
    -- print " "
    -- mapM_ print (debugSolve2 parsed_data2)

    let sol1 = solve1 parsed_data1
    print sol1
    let sol2 = solve2 parsed_data2
    print sol2

-- todo, rewrite with list comprehension
parse :: [Line] -> Grid
parse input = map ((map convert)) input
    where
        convert :: Char -> (Int, SeenCount)
        convert x = ((toInt x), 0)

toInt :: Char -> Int
toInt =  read . pure

parse2 :: [Line] -> Grid
parse2 input = map ((map convert)) input
    where
        convert :: Char -> (Int, SeenCount)
        convert x = ((toInt x), 1)

solve1 :: Grid -> Int
solve1 grid =  (sum . (map (length . (filter (0<))))) gridSeenCount
    where gridSeenCount = map (map snd) markedGrid
          markedGrid = applyAllDirections grid markSeenTrees

-- debug only
debugSolve1 :: Grid -> Grid
debugSolve1 grid =  rotateGridLeft markedGrid
    where markedGrid = applyAllDirections grid markSeenTrees

applyAllDirections :: Grid -> (Grid -> Grid) -> Grid
applyAllDirections grid func = (func . rotateGridLeft . func . rotateGridLeft . func . rotateGridLeft . func) grid

markSeenTrees :: Grid -> Grid
markSeenTrees grid =  map (markSeenTreesRow (-1)) grid

markSeenTreesRow:: Int -> Row -> Row
markSeenTreesRow highestSeenTree []= []
markSeenTreesRow highestSeenTree row
    | thisTreeHeight > highestSeenTree = [incrementSeenCount thisTree] ++ (markSeenTreesRow thisTreeHeight remainingTrees)
    | otherwise = [thisTree] ++ (markSeenTreesRow highestSeenTree remainingTrees)
    where remainingTrees = tail row
          thisTreeHeight = fst thisTree
          thisTree = head row
          incrementSeenCount :: Tree -> Tree
          incrementSeenCount (height, seenCount) = (height, seenCount + 1)

rotateGridLeft :: Grid -> Grid
rotateGridLeft grid = (reverse . transpose) grid

solve2 :: Grid -> Int
solve2 grid =  maximum (map maximum scenicValues)
    where
        scenicValues = map (map snd) scenicGrid
        scenicGrid = applyAllDirections grid calcScenicScore

debugSolve2 :: Grid -> Grid
debugSolve2 grid = rotateGridLeft scenicGrid
    where
        scenicGrid = applyAllDirections grid calcScenicScore

calcScenicScore :: Grid -> Grid
calcScenicScore grid =  map calcScenicScoreRow grid

calcScenicScoreRow :: Row -> Row
calcScenicScoreRow [] = []
calcScenicScoreRow row =  [calcScenicScoreTree row] ++ (calcScenicScoreRow remainingRow)
    where remainingRow = tail row

calcScenicScoreTree :: Row -> Tree
calcScenicScoreTree row = (thisTreeHeight, newScenicScore)
    where newScenicScore = curScenicStore * numTreesCanSee
          numTreesCanSee = calcNumTreesCanSee remainingTrees thisTreeHeight
          remainingTrees = tail row
          curScenicStore = snd thisTree
          thisTreeHeight = fst thisTree
          thisTree = head row

calcNumTreesCanSee :: Row -> Int -> Int
calcNumTreesCanSee [] height = 0
calcNumTreesCanSee row height
    | height > nextTreeHeight = 1 + calcNumTreesCanSee remainingRow height
    | otherwise = 1
    where remainingRow = tail row
          nextTreeHeight = fst nextTree
          nextTree = head row
