module UnitTests (unitTests) where

import           Queue           as Q
import           Test.HUnit
import           Test.QuickCheck


testSize :: Test
testSize = TestCase (do
  assertEqual "for size 0," 0 (size Q.newQueue)
  let q = Q.push 3 Q.newQueue
  assertEqual "for size 1," 1 (size q)
  let q2 = Q.push 7 q
  assertEqual "for size 2," 2 (size q2)
  let q3 = Q.push 7 q2
  assertEqual "for size 3," 3 (size q3))


testPeek :: Test
testPeek = TestCase (do
  let q = Q.push 3 Q.newQueue
  assertEqual "for peek 1," 3 (peek q)
  assertEqual "for peek 2," 3 (peek q))


testPop :: Test
testPop = TestCase (do
  let q = Q.push 6 Q.newQueue
  assertEqual "for pop 1," 6 (fst $ pop q)
  assertEqual "for pop 2," 6 (fst $ pop q)
  let q2 = snd $ pop q
  assertEqual "for size," 0 (size q2))


testToList :: Test
testToList = TestCase (do
  assertEqual "for toList 0," ([] :: [Int]) (toList Q.newQueue)
  let q = Q.push 6 Q.newQueue
  let q2 = Q.push 7 q
  let q3 = Q.push 6 q2
  assertEqual "for toList 1," [6,7,6] (toList q3)
  let q4 = snd $ Q.pop q3
  assertEqual "for toList 2," [7,6] (toList q4))


tests :: Test
tests = TestList [
  TestLabel "testSize" testSize,
  TestLabel "testPeek" testPeek,
  TestLabel "testToList" testToList,
  TestLabel "testPop" testPop ]


unitTests = do
  runTestTT tests
