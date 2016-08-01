module Data.Filterable
  ( class Filterable
  , partitionMap
  , partition
  , filterMap
  , filter
  , eitherBool
  , partitionDefault
  , maybeBool
  , filterDefault
  , partitioned
  , filtered
  , cleared
  ) where

import Prelude (const)
import Control.Category ((<<<), id)
import Control.Bind ((=<<))
import Data.Semigroup ((<>))
import Data.Functor (class Functor, (<$>))
import Data.Foldable (foldl)
import Data.Monoid (class Monoid, mempty)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (partition, mapMaybe, filter) as Array

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
-- | - `filterDefault`
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
eitherBool p x = if p x then Left x else Right x

-- | A default implementation of `partition` using `partitionMap`.
partitionDefault :: forall f a. Filterable f =>
  (a -> Boolean) -> f a -> { no :: f a, yes :: f a }
partitionDefault p xs =
  let o = partitionMap (eitherBool p) xs
  in {no: o.left, yes: o.right}

-- | Upgrade a boolean-style predicate to a maybe-style predicate mapping.
maybeBool :: forall a.
  (a -> Boolean) -> a -> Maybe a
maybeBool p x = if p x then Just x else Nothing

-- | A default implementation of `filter` using `filterMap`.
filterDefault :: forall f a. Filterable f =>
  (a -> Boolean) -> f a -> f a
filterDefault = filterMap <<< maybeBool

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

