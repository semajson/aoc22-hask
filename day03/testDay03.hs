module TestDay02 where

import           Day03
import           Test.HUnit


-- testRoundResult :: Test
test1 =
    TestCase ( assertEqual "Priority val a" 1 (lookupPriority 'a'))
test2 =
    TestCase ( assertEqual "Priority val b" 2 (lookupPriority 'b'))
test3 =
    TestCase ( assertEqual "Priority val Z" 52 (lookupPriority 'Z'))
test4 =
    TestCase ( assertEqual "Priority val z" 26 (lookupPriority 'z'))

test5 =
    TestCase ( assertEqual "errorPriority a" 16 (errorPriority ["vJrwpWtwJgWr","hcsFMMfFFhFp"]))
test6 =
    TestCase ( assertEqual "errorPriority b" 38 (errorPriority ["jqHRNqRjqzjGDLGL","rsFMfFZSrLrFZsSL"]))
test7 =
    TestCase ( assertEqual "getErrorLetters Z" ['p'] (getErrorLetters ["vJrwpWtwJgWr","hcsFMMfFFhFp"]))
test8 =
    TestCase ( assertEqual "getErrorLetters z" ['L', 'L'] (getErrorLetters ["jqHRNqRjqzjGDLGL","rsFMfFZSrLrFZsSL"]))

-- main :: IO Counts
tests =TestList [test1, test2, test3, test4, test5, test6, test7, test8]
