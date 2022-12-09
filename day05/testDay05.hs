module TestDay05 where
import           Day05
import           Test.HUnit


-- testRoundResult :: Test
test1 =
    TestCase ( assertEqual "simple do command test" ["c", "badef", "ghi"] (doCommand (2,1,2) ["abc", "def", "ghi"]))
test2 =
    TestCase ( assertEqual "simple do command test"  ["DNZ", "CM", "P"] (doCommand (1,2,1) ["NZ", "DCM", "P"]))

test3 =
    TestCase ( assertEqual "simple do command test"  ["", "CM", "ZNDP"] (doCommand (3,1,3) ["DNZ", "CM", "P"] ))

test4 =
    TestCase ( assertEqual "simple do command test"  ["MC", "", "ZNDP"] (doCommand (2,2,1) ["", "CM", "ZNDP"]  ))

test5 =
    TestCase ( assertEqual "simple do command test"  ["C", "M", "ZNDP"] (doCommand (1,1,2) ["MC", "", "ZNDP"]))

-- main :: IO Counts
tests =TestList [test1, test2, test3, test4, test5]
