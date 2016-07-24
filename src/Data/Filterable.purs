module Data.Filterable
  ( class Filterable, filterMap, filter
  , maybeBool
  , filterDefault
  ) where

import Control.Category ((<<<))
import Control.Bind ((=<<))
import Data.Functor (class Functor)
import Data.Maybe (Maybe(..))

-- | `Filterable` represents data structures which can be _filtered_.
-- |
-- | - `filterMap` - map over a data structure and filter out
-- | - `filter`
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
  filterMap :: forall a b. (a -> Maybe b) -> f a -> f b
  filter :: forall a. (a -> Boolean) -> f a -> f a

-- | Upgrade a boolean-style predicate to a maybe-style predicate mapping.
maybeBool :: forall a. (a -> Boolean) -> a -> Maybe a
maybeBool p x = if p x then Just x else Nothing

-- | A default implementation of `filter` using `filterMap`.
filterDefault :: forall f a. Filterable f => (a -> Boolean) -> f a -> f a
filterDefault = filterMap <<< maybeBool

instance filterableMaybe :: Filterable Maybe where
  filterMap = (=<<)
  filter p = filterDefault p
