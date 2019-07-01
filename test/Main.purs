module Test.Main where

import Prelude

import Data.Compactable (compact, separate)
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap, partition, partitionMap)
import Data.Identity (Identity(Identity))
import Data.List (List(Nil), (:))
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Witherable (wilt, wither)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual)

testCompactable :: Effect Unit
testCompactable = do
  log "Test compactableMaybe instance" *> do
    let parts1 = separate $ Just ((Left 1) :: Either Int Int)
    assertEqual { actual: parts1.left
                , expected: Just 1
                }
    assertEqual { actual: parts1.right
                , expected: Nothing
                }

    let parts2 = separate $ Just ((Right 2) :: Either Int Int)
    assertEqual { actual: parts2.left
                , expected: Nothing
                }
    assertEqual { actual: parts2.right
                , expected: Just 2
                }

    let parts3 = separate $ (Nothing :: Maybe (Either Int Int))
    assertEqual { actual: parts3.left
                , expected: Nothing
                }
    assertEqual { actual: parts3.right
                , expected: Nothing
                }

  log "Text compactableEither instance" *> do
    assertEqual { actual: compact (Left [1] :: Either (Array Int) (Maybe Int))
                , expected: Left [1]
                }

    assertEqual { actual: compact (Right Nothing :: Either (Array Int) (Maybe Int))
                , expected: Left []
                }

    assertEqual { actual: compact (Right (Just 3) :: Either (Array Int) (Maybe Int))
                , expected: Right 3
                }

    let parts1 = separate (Left [1] :: Either (Array Int) (Either Int Int))
    assertEqual { actual: parts1.left
                , expected: Left [1]
                }
    assertEqual { actual: parts1.right
                , expected: Left [1]
                }

    let parts2 = separate (Right (Left 2) :: Either (Array Int) (Either Int Int))
    assertEqual { actual: parts2.left
                , expected: Right 2
                }
    assertEqual { actual: parts2.right
                , expected: Left []
                }

    let parts3 = separate (Right (Right 3) :: Either (Array Int) (Either Int Int))
    assertEqual { actual: parts3.left
                , expected: Left []
                }
    assertEqual { actual: parts3.right
                , expected: Right 3
                }

  log "Test compactableArray instance" *> do
    let testList = [Left 1, Right 2, Left 3, Right 4, Left 5, Right 6, Left 7, Right 8]
    let parts = separate testList
    assertEqual { actual: parts.left
                , expected: [1, 3, 5, 7]
                }
    assertEqual { actual: parts.right
                , expected: [2, 4, 6, 8]
                }

  log "Test compactableList instance" *> do
    let testList = (Left 1 : Right 2 : Left 3 : Right 4 : Left 5 : Right 6 : Left 7 : Right 8 : Nil)
    let parts = separate testList
    assertEqual { actual: parts.left
                , expected: 1 : 3 : 5 : 7 : Nil
                }
    assertEqual { actual: parts.right
                , expected: 2 : 4 : 6 : 8 : Nil
                }

  log "Test compactableMap instance" *> do
    let m = Map.fromFoldable
    assertEqual { actual: compact $ m [1 /\ Just 1, 2 /\ Nothing, 3 /\ Just 3, 4 /\ Nothing]
                , expected: m [1 /\ 1, 3 /\ 3]
                }

    let parts = separate $ m [1 /\ Left 1, 2 /\ Right 2, 3 /\ Left 3, 4 /\ Right 4]
    assertEqual { actual: parts.left
                 , expected: m [1 /\ 1, 3 /\ 3]
                 }
    assertEqual { actual: parts.right
                , expected: m [2 /\ 2, 4 /\ 4]
                }

testFilterable :: Effect Unit
testFilterable = do
  log "Test filterableMaybe instance" *> do
    assertEqual { actual: filterMap pred (Just 6)
                , expected: Just 60
                }
    assertEqual { actual: filterMap pred (Just 5)
                , expected: Nothing
                }
    assertEqual { actual: filterMap pred Nothing
                , expected: Nothing
                }

    assertEqual { actual: filter (_ > 5) (Just 6)
                , expected: Just 6
                }
    assertEqual { actual: filter (_ > 5) (Just 5)
                , expected: Nothing
                }
    assertEqual { actual: filter (_ > 5) Nothing
                , expected: Nothing
                }

  log "Test filterableList instance" *> do
    let testlist = (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : Nil)
    assertEqual { actual: filterMap pred testlist
                , expected: 60 : 70 : 80 : 90 : Nil
                }
    assertEqual { actual: filter (_ > 5) testlist
                , expected: 6 : 7 : 8 : 9 : Nil
                }
    assertEqual { actual: partition (_ > 5) testlist
                , expected: { no: (1 : 2 : 3 : 4 : 5 : Nil), yes: (6 : 7 : 8 : 9 : Nil) }
                }
    assertEqual { actual: (partitionMap Right $ (1 : 2 : 3 : 4 : 5 : Nil)).right
                , expected: 1 : 2 : 3 : 4 : 5 : Nil
                }
    assertEqual { actual: (partitionMap Left $ (1 : 2 : 3 : 4 : 5 : Nil)).left
                , expected: 1 : 2 : 3 : 4 : 5 : Nil
                }

  log "Test filterableArray instance" *> do
    assertEqual { actual: filterMap pred [1,2,3,4,5,6,7,8,9]
                , expected: [60,70,80,90]
                }
    assertEqual { actual: filter (_ > 5) [1,2,3,4,5,6,7,8,9]
                , expected: [6,7,8,9]
                }
    assertEqual { actual: partition (_ > 5) [1,2,3,4,5,6,7,8,9]
                , expected: { no: [1,2,3,4,5], yes: [6,7,8,9] }
                }
  log "Test filterableMap instance" *> do
    let predE x = if x > 5 then Right (x * 10) else Left x
    let m = Map.fromFoldable
    let xs = m [4 /\ 4, 5 /\ 5, 6 /\ 6, 7 /\ 7]
    assertEqual { actual: filterMap pred xs
                , expected: m [6 /\ 60, 7 /\ 70]
                }
    assertEqual { actual: filter (_ > 5) xs
                , expected: m [6 /\ 6, 7 /\ 7]
                }
    assertEqual { actual: partition (_ > 5) xs
                , expected: { no: m [4 /\ 4, 5 /\ 5], yes: m [6 /\ 6, 7 /\ 7] }
                }
    assertEqual { actual: partitionMap predE xs
                , expected: { left: m [4 /\ 4, 5 /\ 5], right: m [6 /\ 60, 7 /\ 70] }
                }
  where
    pred x = if x > 5 then Just (x * 10) else Nothing

testWitherable :: Effect Unit
testWitherable = do
   log "Test witherableMaybe instance" *> do
     assertEqual { actual: map _.right (wilt predE (Just 6))
                 , expected: Identity (Just 60)
                 }
     assertEqual { actual: map _.left (wilt predE (Just 5))
                 , expected: Identity (Just 5)
                 }
     assertEqual { actual: map _.right (wilt predE Nothing)
                 , expected: Identity Nothing
                 }

     assertEqual { actual: wither predM (Just 6)
                 , expected: Identity (Just 60)
                 }
     assertEqual { actual: wither predM (Just 5)
                 , expected: Identity Nothing
                 }
     assertEqual { actual: wither predM Nothing
                 , expected: Identity Nothing
                 }

   log "Test witherableEither instance" *> do
     assertEqual { actual:  map _.right (wilt predE (Right 6 :: Either (Array Int) Int))
                 , expected: Identity (Right 60)
                 }
     assertEqual { actual: map _.left (wilt predE (Right 5 :: Either (Array Int) Int))
                 , expected: Identity (Right 5)
                 }
     assertEqual { actual: map _.right (wilt predE (Left [] :: Either (Array Int) Int))
                 , expected: Identity (Left [])
                 }

     assertEqual { actual: wither predM (Just 6)
                 , expected: Identity (Just 60)
                 }
     assertEqual { actual: wither predM (Just 5)
                 , expected: Identity Nothing
                 }
     assertEqual { actual: wither predM Nothing
                 , expected: Identity Nothing
                 }

   log "Test witherableList instance" *> do
     let testlist = (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : Nil)
     let resultWilt = wilt predE testlist
     assertEqual { actual: map _.right resultWilt
                 , expected: Identity (60 : 70 : 80 : 90 : Nil)
                 }
     assertEqual { actual: map _.left resultWilt
                 , expected: Identity (1 : 2 : 3 : 4 : 5 : Nil)
                 }
     assertEqual { actual: map _.right (wilt predE Nil)
                 , expected: Identity Nil
                 }

     assertEqual { actual: wither predM testlist
                 , expected: Identity (60 : 70 : 80 : 90 : Nil)
                 }
     assertEqual { actual: wither predM Nil
                 , expected: Identity Nil
                 }

   log "Test witherableArray instance" *> do
     let testarray = [1, 2, 3, 4, 5, 6, 7, 8, 9]
     let resultWilt = wilt predE testarray
     assertEqual { actual: map _.right resultWilt
                 , expected: Identity [60, 70, 80, 90]
                 }
     assertEqual { actual: map _.left resultWilt
                 , expected: Identity [1, 2, 3, 4, 5]
                 }
     assertEqual { actual: map _.right (wilt predE [])
                 , expected: Identity []
                 }

     assertEqual { actual: wither predM testarray
                 , expected: Identity [60, 70, 80, 90]
                 }
     assertEqual { actual: wither predM [], expected: Identity [] }

   log "Test witherableMap instance" *> do
     let m = Map.fromFoldable
     let xs = m [4 /\ 4, 5 /\ 5, 6 /\ 6, 7 /\ 7]
     let resultWilt = wilt predE xs
     assertEqual { actual: map _.right resultWilt
                 , expected: Identity (m [6 /\ 60, 7 /\ 70])
                 }
     assertEqual { actual: map _.left resultWilt
                 , expected: Identity (m [4 /\ 4, 5 /\ 5])
                 }

     assertEqual { actual: wither predM xs
                 , expected: Identity (m [6 /\ 60, 7 /\ 70])
                 }

   where
     predM x = if x > 5 then Identity (Just (x * 10)) else Identity Nothing
     predE x = if x > 5 then Identity (Right (x * 10)) else Identity (Left x)

main :: Effect Unit
main = do
  testCompactable
  testFilterable
  testWitherable
  log "All done!"
