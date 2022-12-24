module TestDay09 where
import           Day09
import           Test.HUnit


-- testRoundResult :: Test
test1 =
    TestCase ( assertEqual "simple do command test" ["c", "badef", "ghi"] (doCommand (2,1,2) ["abc", "def", "ghi"]))


-- main :: IO Counts
tests =TestList [test1]
