module Data.Witherable
  ( class Witherable
  , wilt
  , wither
  , partitionMapByWilt
  , filterMapByWither
  , traverseByWither
  , wilted
  , module Data.Filterable
  ) where

import Control.Category ((<<<), id)
import Control.Applicative (class Applicative, pure)
import Data.Identity (Identity(..), runIdentity)
import Data.Filterable (class Filterable)
import Data.Functor (map)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)

-- | `Witherable` represents data structures which can be _partitioned_ with
-- | effects in some `Applicative` functor.
-- |
-- | - `wilt` - partition a structure with effects
-- | - `wither` - filter a structure  with effects
-- |
-- | Laws:
-- |
-- | - Identity: `wither (pure <<< Just) ≡ pure`
-- | - Composition: `Compose <<< map (wither f) <<< wither g ≡ wither (Compose <<< map (wither f) <<< g)`
-- |
-- | Superclass equivalences:
-- |
-- | - `partitionMap p = runIdentity <<< wilt (Identity <<< p)`
-- | - `filterMap p = runIdentity <<< wither (Identity <<< p)`
-- | - `traverse f ≡ wither (map Just <<< f)`
-- |
-- | Default implementations are provided by the following functions:
-- |
-- | - `partitionMapByWilt`
-- | - `filterMapByWither`
-- | - `traverseByWither`
class (Filterable t, Traversable t) <= Witherable t where
  wilt :: forall m a l r.
    Applicative m => (a -> m (Either l r)) -> t a -> m { left :: t l,
                                                         right :: t r }

  wither :: forall m a b.
    Applicative m => (a -> m (Maybe b)) -> t a -> m (t b)

-- | A default implementation of `parititonMap` given a `Witherable`.
partitionMapByWilt :: forall t a l r.
  Witherable t => (a -> Either l r) -> t a -> { left :: t l, right :: t r }
partitionMapByWilt p = runIdentity <<< wilt (Identity <<< p)

-- | A default implementation of `filterMap` given a `Witherable`.
filterMapByWither :: forall t a b.
  Witherable t => (a -> Maybe b) -> t a -> t b
filterMapByWither p = runIdentity <<< wither (Identity <<< p)

-- | A default implementation of `traverse` given a `Witherable`.
traverseByWither :: forall t m a b.
  (Witherable t, Applicative m) => (a -> m b) -> t a -> m (t b)
traverseByWither f = wither (map Just <<< f)

wilted :: forall t m l r. (Witherable t, Applicative m)
  => t (m (Either l r)) -> m { left :: t l, right :: t r }
wilted = wilt id

instance witherableMaybe :: Witherable Maybe where
  wilt p Nothing = pure { left: Nothing, right: Nothing }
  wilt p (Just x) = map go (p x) where
    go (Left l) = { left: Just l, right: Nothing }
    go (Right r) = { left: Nothing, right: Just r }

  wither p Nothing = pure Nothing
  wither p (Just x) = p x
