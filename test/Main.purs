module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Filterable (filter, filterMap, partition, partitionMap)
import Data.Identity (Identity(Identity))
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Map (fromFoldable) as Map
import Data.Tuple (Tuple(..))
import Data.Witherable (wither)
import Test.Assert (ASSERT, assert)

testEqNoYes :: ∀ a. (Ord a) => { no :: a, yes :: a } -> { no :: a, yes :: a } -> Boolean
testEqNoYes { no: n1, yes: y1 } { no: n2, yes: y2 } =
    n1 == n2 && y1 == y2

testEqLeftRight :: ∀ a. (Ord a) => { left :: a, right :: a } -> { left :: a, right :: a } -> Boolean
testEqLeftRight { left: l1, right: r1 } { left: l2, right: r2 } =
    l1 == l2 && r1 == r2


main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "Test filterableMaybe instance" *> do
    let pred x = if x > 5 then Just (x * 10) else Nothing
    assert $ filterMap pred (Just 6) == Just 60
    assert $ filterMap pred (Just 5) == Nothing
    assert $ filterMap pred Nothing == Nothing

    assert $ filter (_ > 5) (Just 6) == Just 6
    assert $ filter (_ > 5) (Just 5) == Nothing
    assert $ filter (_ > 5) Nothing == Nothing

  log "Test witherableMaybe instance" *> do
    let pred x = if x > 5 then Identity (Just (x * 10)) else Identity Nothing
    assert $ wither pred (Just 6) == Identity (Just 60)
    assert $ wither pred (Just 5) == Identity Nothing
    assert $ wither pred Nothing == Identity Nothing

  log "Test filterableList instance" *> do
    let pred x = if x > 5 then Just (x * 10) else Nothing
    let testlist = (1 : 2 : 3 : 4 : 5 : 6 : 7 : 8 : 9 : Nil)
    assert $ filterMap pred testlist == (60 : 70 : 80 : 90 : Nil)
    assert $ filter (_ > 5) testlist == (6 : 7 : 8 : 9 : Nil)
    assert $ partition (_ > 5) testlist `testEqNoYes` { no: (1 : 2 : 3 : 4 : 5 : Nil), yes: (6 : 7 : 8 : 9 : Nil)}
    assert $ (partitionMap Right $ (1 : 2 : 3 : 4 : 5 : Nil)).right == (1 : 2 : 3 : 4 : 5 : Nil)
    assert $ (partitionMap Left $ (1 : 2 : 3 : 4 : 5 : Nil)).left == (1 : 2 : 3 : 4 : 5 : Nil)

  log "Test filterableArray instance" *> do
    let pred x = if x > 5 then Just (x * 10) else Nothing
    assert $ filterMap pred [1,2,3,4,5,6,7,8,9] == [60,70,80,90]
    assert $ filter (_ > 5) [1,2,3,4,5,6,7,8,9] == [6,7,8,9]
    assert $ partition (_ > 5) [1,2,3,4,5,6,7,8,9] `testEqNoYes` { no: [1,2,3,4,5], yes: [6,7,8,9]}

  log "Test filterableMap instance" *> do
    let pred x = if x > 2 then Just (x * 10) else Nothing
    let predE x = if x > 2 then Right (x * 10) else Left x
    let m = Map.fromFoldable
    let xs = m [Tuple 1 1, Tuple 2 2, Tuple 3 3, Tuple 4 4]
    assert $ filterMap pred xs == m [Tuple 3 30, Tuple 4 40]
    assert $ filter (_ > 2) xs == m [Tuple 3 3, Tuple 4 4]
    assert $ partition (_ > 2) xs `testEqNoYes` { no: m [Tuple 1 1, Tuple 2 2]
                                                , yes: m [Tuple 3 3, Tuple 4 4] }
    assert $ partitionMap predE xs `testEqLeftRight` { left: m [Tuple 1 1, Tuple 2 2]
                                                     , right: m [Tuple 3 30, Tuple 4 40] }

  log "All done!"
