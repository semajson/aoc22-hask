module TestDay02 where

import           Day02
import           Test.HUnit


-- testRoundResult :: Test
test1 =
    TestCase ( assertEqual "Test for draw " 3 (roundResult["A", "X"]))
test2 =
    TestCase ( assertEqual "Tests for opp wins" 0 (roundResult["A", "Z"]))
test3 =
    TestCase ( assertEqual "Tests for our wins" 6 (roundResult["A", "Y"]))

-- main :: IO Counts
tests =TestList [test1, test2, test3]
