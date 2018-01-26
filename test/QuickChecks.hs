{-# LANGUAGE FlexibleInstances #-}

module QuickChecks (quickChecks) where

import           Queue           as Q
import           Test.QuickCheck

instance Arbitrary (Queue Int) where
  arbitrary = do
    values <- arbitrary :: Gen [Int]
    return (foldr Q.push Q.newQueue values)

prop_eq :: [Int] -> Bool
prop_eq vals = q1 == q2
  where q1 = foldr Q.push Q.newQueue vals
        q2 = foldr Q.push Q.newQueue vals

prop_peekPop :: Queue Int -> Bool
prop_peekPop q = (size q == 0) || peek q == fst (pop q)

prop_pushPop :: Queue Int -> Int -> Bool
prop_pushPop q el = (size $ snd $ Q.pop $ Q.push el q) == size q

quickChecks = do
  quickCheck prop_eq
  quickCheck prop_peekPop
  quickCheck prop_pushPop
