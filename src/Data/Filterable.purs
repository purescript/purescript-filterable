module Data.Filterable
  ( class Filterable
  , partitionMap
  , partition
  , filterMap
  , filter
  , eitherBool
  , partitionDefault
  , partitionDefaultFilter
  , partitionDefaultFilterMap
  , maybeBool
  , filterDefault
  , filterDefaultPartition
  , filterDefaultPartitionMap
  , partitioned
  , filtered
  , cleared
  ) where

import Control.Bind ((=<<))
import Control.Category ((<<<), id)
import Data.Array (partition, mapMaybe, filter) as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Functor (class Functor)
import Data.HeytingAlgebra (not)
import Data.List (List(..), filter, mapMaybe) as List
import Data.Map (Map, empty, insert, alter, toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Prelude (const, class Ord)

-- | `Filterable` represents data structures which can be _partitioned_/_filtered_.
-- |
-- | - `partitionMap` - partition a data structure based on an either predicate.
-- | - `partition` - partition a data structure based on boolean predicate.
-- | - `filterMap` - map over a data structure and filter based on a maybe.
-- | - `filter` - filter a data structure based on a boolean.
-- |
-- | Laws:
-- | - `map f ≡ filterMap (Just <<< f)`
-- | - `filter ≡ filterMap <<< maybeBool`
-- | - `filterMap p ≡ filter (isJust <<< p)`
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `partitionDefault`
-- | - `partitionDefaultFilter`
-- | - `partitionDefaultFilterMap`
-- | - `filterDefault`
-- | - `filterDefaultPartition`
-- | - `filterDefaultPartitionMap`
class (Functor f) <= Filterable f where
  partitionMap :: forall a l r.
    (a -> Either l r) -> f a -> { left :: f l, right :: f r }

  partition :: forall a.
    (a -> Boolean) -> f a -> { no :: f a, yes :: f a }

  filterMap :: forall a b.
    (a -> Maybe b) -> f a -> f b

  filter :: forall a.
    (a -> Boolean) -> f a -> f a

-- | Upgrade a boolean-style predicate to an either-style predicate mapping.
eitherBool :: forall a.
  (a -> Boolean) -> a -> Either a a
eitherBool p x = if p x then Right x else Left x

-- | Upgrade a boolean-style predicate to a maybe-style predicate mapping.
maybeBool :: forall a.
  (a -> Boolean) -> a -> Maybe a
maybeBool p x = if p x then Just x else Nothing

-- | A default implementation of `partition` using `partitionMap`.
partitionDefault :: forall f a. Filterable f =>
  (a -> Boolean) -> f a -> { no :: f a, yes :: f a }
partitionDefault p xs =
  let o = partitionMap (eitherBool p) xs
  in {no: o.left, yes: o.right}

-- | A default implementation of `partition` using `filter`. Note that this is
-- | almost certainly going to be suboptimal compared to direct implementations.
partitionDefaultFilter :: forall f a. Filterable f =>
  (a -> Boolean) -> f a -> { no :: f a, yes :: f a }
partitionDefaultFilter p xs = { yes: filter p xs, no: filter (not p) xs }

-- | A default implementation of `partition` using `filterMap`. Note that this
-- | is almost certainly going to be suboptimal compared to direct
-- | implementations.
partitionDefaultFilterMap :: forall f a. Filterable f =>
  (a -> Boolean) -> f a -> { no :: f a, yes :: f a }
partitionDefaultFilterMap p xs =
  { yes: filterMap (maybeBool p) xs
  , no: filterMap (maybeBool (not p)) xs
  }

-- | A default implementation of `filter` using `filterMap`.
filterDefault :: forall f a. Filterable f =>
  (a -> Boolean) -> f a -> f a
filterDefault = filterMap <<< maybeBool

-- | A default implementation of `filter` using `partition`.
filterDefaultPartition :: forall f a. Filterable f =>
  (a -> Boolean) -> f a -> f a
filterDefaultPartition p xs = (partition p xs).yes

-- | A default implementation of `filter` using `partitionMap`.
filterDefaultPartitionMap :: forall f a. Filterable f =>
  (a -> Boolean) -> f a -> f a
filterDefaultPartitionMap p xs = (partitionMap (eitherBool p) xs).right

partitioned :: forall f l r. Filterable f =>
  f (Either l r) -> { left :: f l, right :: f r }
partitioned = partitionMap id

-- | Filter out all the `Nothing` values.
filtered :: forall f a. Filterable f =>
  f (Maybe a) -> f a
filtered = filterMap id

-- | Filter out all values.
cleared :: forall f a b. Filterable f =>
  f a -> f b
cleared = filterMap (const Nothing)

instance filterableArray :: Filterable Array where
  partitionMap p = foldl go {left: [], right: []} where
    go acc x = case p x of
      Left l -> acc { left = acc.left <> [l] }
      Right r -> acc { right = acc.right <> [r] }

  partition = Array.partition

  filterMap = Array.mapMaybe

  filter = Array.filter

instance filterableMaybe :: Filterable Maybe where
  partitionMap p Nothing = { left: Nothing, right: Nothing }
  partitionMap p (Just x) = case p x of
    Left a -> { left: Just a, right: Nothing }
    Right b -> { left: Nothing, right: Just b }

  partition p = partitionDefault p

  filterMap = (=<<)

  filter p = filterDefault p

instance filterableEither :: Monoid m => Filterable (Either m) where
  partitionMap p (Left x) = { left: Left x, right: Left x }
  partitionMap p (Right x) = case p x of
    Left a -> { left: Right a, right: Left mempty }
    Right b -> { left: Left mempty, right: Right b }

  partition p = partitionDefault p

  filterMap p (Left l) = Left l
  filterMap p (Right r) = case p r of
    Nothing -> Left mempty
    Just x -> Right x

  filter p = filterDefault p

instance filterableList :: Filterable List.List where
  -- partitionMap :: forall a l r. (a -> Either l r) -> List a -> { left :: List l, right :: List r }
  partitionMap p xs = foldr select { left: List.Nil, right: List.Nil } xs
    where
        select x { left, right } = case p x of
                                     Left l -> { left: List.Cons l left, right }
                                     Right r -> { left, right: List.Cons r right }

  -- partition :: forall a. (a -> Boolean) -> List a -> { no :: List a, yes :: List a }
  partition p xs = foldr select { no: List.Nil, yes: List.Nil } xs
    where
        -- select :: (a -> Boolean) -> a -> { no :: List a, yes :: List a } -> { no :: List a, yes :: List a }
        select x { no, yes } = if p x
                                 then { no, yes: List.Cons x yes }
                                 else { no: List.Cons x no, yes }

  -- filterMap :: forall a b. (a -> Maybe b) -> List a -> List b
  filterMap p = List.mapMaybe p

  -- filter :: forall a. (a -> Boolean) -> List a -> List a
  filter = List.filter

instance filterableMap :: Ord k => Filterable (Map.Map k) where
  partitionMap p xs =
    foldr select { left: Map.empty, right: Map.empty } (toList xs)
    where
      toList :: forall v. Map.Map k v -> List.List (Tuple k v)
      toList = Map.toUnfoldable

      select (Tuple k x) { left, right } = case p x of
        Left l -> { left: Map.insert k l left, right }
        Right r -> { left, right: Map.insert k r right }

  partition p = partitionDefault p

  filterMap p xs =
    foldr select Map.empty (toList xs)
    where
      toList :: forall v. Map.Map k v -> List.List (Tuple k v)
      toList = Map.toUnfoldable

      select (Tuple k x) m = Map.alter (const (p x)) k m

  filter p = filterDefault p
