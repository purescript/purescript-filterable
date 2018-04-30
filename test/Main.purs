module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Compactable (compact, separate)
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap, partition, partitionMap)
import Data.Identity (Identity(Identity))
import Data.List (List(Nil), (:))
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Witherable (wilt, wither)
import Test.Assert (assert)

testEqNoYes :: ∀ a. (Ord a) => { no :: a, yes :: a } -> { no :: a, yes :: a } -> Boolean
testEqNoYes { no: n1, yes: y1 } { no: n2, yes: y2 } =
    n1 == n2 && y1 == y2

testEqLeftRight :: ∀ a. (Ord a) => { left :: a, right :: a } -> { left :: a, right :: a } -> Boolean
testEqLeftRight { left: l1, right: r1 } { left: l2, right: r2 } =
    l1 == l2 && r1 == r2

testCompactable :: Effect Unit
testCompactable = do
  log "Test compactableMaybe instance" *> do
    let parts1 = separate $ Just ((Left 1) :: Either Int Int)
    assert $ parts1.left == Just 1
    assert $ parts1.right == Nothing

    let parts2 = separate $ Just ((Right 2) :: Either Int Int)
    assert $ parts2.left == Nothing
    assert $ parts2.right == Just 2

    let parts3 = separate $ Nothing :: Maybe (Either Int Int)
    assert $ parts3.left == Nothing
    assert $ parts3.right == Nothing

  log "Text compactableEither instance" *> do
    let e1 = (Left [1] :: Either (Array Int) (Maybe Int))
    assert $ compact e1 == Left [1]

    let e2 = (Right Nothing :: Either (Array Int) (Maybe Int))
    assert $ compact e2 == Left []

    let e3 = (Right (Just 3) :: Either (Array Int) (Maybe Int))
    assert $ compact e3 == Right 3

    let parts1 = separate (Left [1] :: Either (Array Int) (Either Int Int))
    assert $ parts1.left == Left [1]
    assert $ parts1.right == Left [1]

    let parts2 = separate (Right (Left 2) :: Either (Array Int) (Either Int Int))
    assert $ parts2.left == Right 2
    assert $ parts2.right == Left []

    let parts3 = separate (Right (Right 3) :: Either (Array Int) (Either Int Int))
    assert $ parts3.left == Left []
    assert $ parts3.right == Right 3

  log "Test compactableArray instance" *> do
    let testList = [Left 1, Right 2, Left 3, Right 4, Left 5, Right 6, Left 7, Right 8]
    let parts = separate testList
    assert $ parts.left == [1, 3, 5, 7]
    assert $ parts.right == [2, 4, 6, 8]

  log "Test compactableList instance" *> do
    let testList = (Left 1 : Right 2 : Left 3 : Right 4 : Left 5 : Right 6 : Left 7 : Right 8 : Nil)
    let parts = separate testList
    assert $ parts.left == (1 : 3 : 5 : 7 : Nil)
    assert $ parts.right == (2 : 4 : 6 : 8 : Nil)

  log "Test compactableMap instance" *> do
    let m = Map.fromFoldable
    let testCompactMap = m [1 /\ Just 1, 2 /\ Nothing, 3 /\ Just 3, 4 /\ Nothing]
    let comparisonMapOdds = m [1 /\ 1, 3 /\ 3]
    assert $ compact testCompactMap == comparisonMapOdds

    let testSeparateMap = m [1 /\ Left 1, 2 /\ Right 2, 3 /\ Left 3, 4 /\ Right 4]
    let comparisonMapEvens = m [2 /\ 2, 4 /\ 4]
    let parts = separate testSeparateMap
    assert $ parts.left == comparisonMapOdds
    assert $ parts.right == comparisonMapEvens

testFilterable :: Effect Unit
testFilterable = do
  log "Test filterableMaybe instance" *> do
    assert $ filterMap pred (Just 6) == Just 60
    assert $ filterMap pred (Just 5) == Nothing
    assert $ filterMap pred Nothing == Nothing

    assert $ filter (_ > 5) (Just 6) == Just 6
    assert $ filter (_ > 5) (Just 5) == Nothing
    assert $ filter (_ > 5) Nothing == Nothing

  log "Test filterableList instance" *> do
    let testlist = (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : Nil)
    assert $ filterMap pred testlist == (60 : 70 : 80 : 90 : Nil)
    assert $ filter (_ > 5) testlist == (6 : 7 : 8 : 9 : Nil)
    assert $ partition (_ > 5) testlist `testEqNoYes` { no: (1 : 2 : 3 : 4 : 5 : Nil), yes: (6 : 7 : 8 : 9 : Nil)}
    assert $ (partitionMap Right $ (1 : 2 : 3 : 4 : 5 : Nil)).right == (1 : 2 : 3 : 4 : 5 : Nil)
    assert $ (partitionMap Left $ (1 : 2 : 3 : 4 : 5 : Nil)).left == (1 : 2 : 3 : 4 : 5 : Nil)

  log "Test filterableArray instance" *> do
    assert $ filterMap pred [1,2,3,4,5,6,7,8,9] == [60,70,80,90]
    assert $ filter (_ > 5) [1,2,3,4,5,6,7,8,9] == [6,7,8,9]
    assert $ partition (_ > 5) [1,2,3,4,5,6,7,8,9] `testEqNoYes` { no: [1,2,3,4,5], yes: [6,7,8,9]}
  log "Test filterableMap instance" *> do
    let predE x = if x > 5 then Right (x * 10) else Left x
    let m = Map.fromFoldable
    let xs = m [4 /\ 4, 5 /\ 5, 6 /\ 6, 7 /\ 7]
    assert $ filterMap pred xs == m [6 /\ 60, 7 /\ 70]
    assert $ filter (_ > 5) xs == m [6 /\ 6, 7 /\ 7]
    assert $ partition (_ > 5) xs `testEqNoYes` { no: m [4 /\ 4, 5 /\ 5]
                                                , yes: m [6 /\ 6, 7 /\ 7] }
    assert $ partitionMap predE xs `testEqLeftRight` { left: m [4 /\ 4, 5 /\ 5]
                                                     , right: m [6 /\ 60, 7 /\ 70] }
  where
    pred x = if x > 5 then Just (x * 10) else Nothing

testWitherable :: Effect Unit
testWitherable = do
   log "Test witherableMaybe instance" *> do
     assert $ map _.right (wilt predE (Just 6)) == Identity (Just 60)
     assert $ map _.left (wilt predE (Just 5)) == Identity (Just 5)
     assert $ map _.right (wilt predE Nothing) == Identity Nothing

     assert $ wither predM (Just 6) == Identity (Just 60)
     assert $ wither predM (Just 5) == Identity Nothing
     assert $ wither predM Nothing == Identity Nothing

   log "Test witherableEither instance" *> do
     assert $ map _.right (wilt predE (Right 6 :: Either (Array Int) Int)) == Identity (Right 60)
     assert $ map _.left (wilt predE (Right 5 :: Either (Array Int) Int)) == Identity (Right 5)
     assert $ map _.right (wilt predE (Left [] :: Either (Array Int) Int)) == Identity (Left [])

     assert $ wither predM (Just 6) == Identity (Just 60)
     assert $ wither predM (Just 5) == Identity Nothing
     assert $ wither predM Nothing == Identity Nothing

   log "Test witherableList instance" *> do
     let testlist = (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : Nil)
     let resultWilt = wilt predE testlist
     assert $ map _.right resultWilt == Identity (60 : 70 : 80 : 90 : Nil)
     assert $ map _.left resultWilt == Identity (1 : 2 : 3 : 4 : 5 : Nil)
     assert $ map _.right (wilt predE Nil) == Identity Nil

     assert $ wither predM testlist == Identity (60 : 70 : 80 : 90 : Nil)
     assert $ wither predM Nil == Identity Nil

   log "Test witherableArray instance" *> do
     let testarray = [1, 2, 3, 4, 5, 6, 7, 8, 9]
     let resultWilt = wilt predE testarray
     assert $ map _.right resultWilt == Identity [60,  70, 80, 90]
     assert $ map _.left resultWilt == Identity [1, 2, 3, 4, 5]
     assert $ map _.right (wilt predE []) == Identity []

     assert $ wither predM testarray == Identity [60, 70, 80, 90]
     assert $ wither predM [] == Identity []

   log "Test witherableMap instance" *> do
     let m = Map.fromFoldable
     let xs = m [4 /\ 4, 5 /\ 5, 6 /\ 6, 7 /\ 7]
     let resultWilt = wilt predE xs
     assert $ map _.right resultWilt == Identity (m [6 /\ 60, 7 /\ 70])
     assert $ map _.left resultWilt == Identity (m [4 /\ 4, 5 /\ 5])

     assert $ wither predM xs == Identity (m [6 /\ 60, 7 /\ 70])

   where
     predM x = if x > 5 then Identity (Just (x * 10)) else Identity Nothing
     predE x = if x > 5 then Identity (Right (x * 10)) else Identity (Left x)

main :: Effect Unit
main = do
  testCompactable
  testFilterable
  testWitherable
  log "All done!"
