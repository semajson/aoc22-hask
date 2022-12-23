module TestDay08 where
import           Day08
import           Test.HUnit


-- testRoundResult :: Test
test1 =
    TestCase ( assertEqual "calcScenicScoreTree test 1" (5,1) (calcScenicScoreTree [(5,0), (5,0), (3,0)]))
test2 =
    TestCase ( assertEqual "calcScenicScoreTree test 2" (5,2) (calcScenicScoreTree [(5,0), (3,0), (6,0)]))
test3 =
    TestCase ( assertEqual "calcScenicScoreTree test 3" (5,2) (calcScenicScoreTree [(5,0), (3,0), (5,0)]))
test4 =
    TestCase ( assertEqual "calcScenicScoreTree test 3" (5,0) (calcScenicScoreTree [(5,0)]))
-- main :: IO Counts
tests =TestList [test1, test2, test3, test4]
