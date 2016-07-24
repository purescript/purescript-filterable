module Data.Witherable
  ( class Witherable, wither
  , filterMapByWither
  , traverseByWither
  , module Data.Filterable
  ) where

import Prelude ((<<<))
import Control.Applicative (class Applicative, pure)
import Data.Identity (Identity(..), runIdentity)
import Data.Filterable (class Filterable)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)

-- | `Witherable` represents data structures which can be _traversed_,
-- | and _filtered_ accumulating results and effects in some `Applicative`
-- | functor.
-- |
-- | - `wither` - filter a structure with effects
-- |
-- | Laws:
-- |
-- | - Identity: `wither (pure <<< Just) ≡ pure`
-- | - Composition: `Compose <<< map (wither f) <<< wither g ≡ wither (Compose <<< map (wither f) <<< g)`
-- |
-- | Superclass equivalences:
-- |
-- | - `traverse f ≡ wither (map Just <<< f)`
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `traverseByWither`
-- | - `filterMapByWither`
class (Filterable t, Traversable t) <= Witherable t where
  wither :: forall a b m.
    Applicative m => (a -> m (Maybe b)) -> t a -> m (t b)

-- | A default implementation of `filterMap` given a `Witherable`.
filterMapByWither :: forall t a b.
  Witherable t => (a -> Maybe b) -> t a -> t b
filterMapByWither p = runIdentity <<< wither (Identity <<< p)

-- | A default implementation of `traverse` given a `Witherable`.
traverseByWither :: forall t m a b.
  (Witherable t, Applicative m) => (a -> m b) -> t a -> m (t b)
traverseByWither f = wither (map Just <<< f)

instance witherableMaybe :: Witherable Maybe where
  wither p Nothing = pure Nothing
  wither p (Just x) = p x
