module TestDay10 where
import           Day10
import           Test.HUnit


-- testRoundResult :: Test
test1 =
    TestCase ( assertEqual "simple do command test" ((6,0), [(5,0), (4,0), (3,0)]) (moveDirectionMulti "R" (5,0) [(4,0), (3,0), (2,0)]))


-- main :: IO Counts
tests =TestList [test1]
