module TestDay07 where
import           Day07
import           Test.HUnit


-- testRoundResult :: Test
test1 =
    TestCase ( assertEqual "Simple tst jsut 1 ls" [("/", ["dir a", "14848514 b.txt"])] (parse  ["$ cd /", "$ ls", "dir a", "14848514 b.txt"] ))
test2 =
    TestCase ( assertEqual "Simple 1 level dep" [ ("/a",  ["29116 f"]), ("/", ["dir a", "dir d", "14848514 b.txt"])] (parse  ["$ cd /", "$ ls", "dir a", "dir d", "14848514 b.txt", "$ cd a", "$ ls", "29116 f"] ))

test3 =
    TestCase ( assertEqual "Simple 1 level dep" [ ("/d", ["584 i"]), ("/a",  ["29116 f"]), ("/", ["dir a", "dir d", "14848514 b.txt"])] (parse  ["$ cd /", "$ ls", "dir a", "dir d", "14848514 b.txt", "$ cd a", "$ ls", "29116 f", "$ cd ..", "$ cd d", "$ ls", "584 i"] ))


test4 =
    TestCase ( assertEqual "Simple 1 level dep" [ ("/d", ["584 i"]), ("/a/b",  ["1234 b.txt"]), ("/a",  ["29116 f", "dir b"]), ("/", ["dir a", "dir d", "14848514 b.txt"])] (parse  ["$ cd /", "$ ls", "dir a", "dir d", "14848514 b.txt", "$ cd a", "$ ls", "29116 f", "dir b", "$ cd b", "$ ls ", "1234 b.txt","$ cd ..", "$ cd ..", "$ cd d", "$ ls", "584 i"] ))

-- test3 =
--     TestCase ( assertEqual "simple do command test"  ["", "CM", "ZNDP"] (doCommand (3,1,3) ["DNZ", "CM", "P"] ))


-- main :: IO Counts
tests =TestList [test1, test2,test3, test4]
