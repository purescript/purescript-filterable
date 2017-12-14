module Data.Compactable
  ( class Compactable
  , compact
  , separate
  , compactDefault
  , separateDefault
  ) where

import Control.Applicative (class Applicative, class Apply, apply, pure)
import Control.Bind (class Bind, bind, join)
import Data.Array as Array
import Data.Either (Either(Right, Left), hush, note)
import Data.Foldable (class Foldable, foldl)
import Data.Function (($))
import Data.Functor (class Functor, map, (<$>))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty, (<>))
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))
import Prelude (class Ord, unit, (<<<))

class Compactable f where
  compact :: forall a. f (Maybe a) -> f a
  separate :: forall l r. f (Either l r) -> { left :: f l, right :: f r }

compactDefault :: forall f a. Functor f => Compactable f => f (Maybe a) -> f a
compactDefault = _.right <<< separate <<< map (note unit)

separateDefault
  :: forall f l r
   . Functor f
  => Compactable f
  => f (Either l r) -> { left :: f l, right :: f r}
separateDefault xs = { left: compact $ (hush <<< swapEither) <$> xs
                     , right: compact $ hush <$> xs
                     }
  where
    swapEither e = case e of
      Left x  -> Right x
      Right y -> Left y

instance compactableMaybe :: Compactable Maybe where
  compact = join

  separate Nothing = { left: Nothing, right: Nothing }
  separate (Just e) = case e of
    Left l  -> { left: Just l, right: Nothing }
    Right r -> { left: Nothing, right: Just r }

instance compactableEither :: Monoid m => Compactable (Either m) where
  compact (Left m) = Left m
  compact (Right m) = case m of
    Just v  -> Right v
    Nothing -> Left mempty

  separate (Left x) = { left: Left x, right: Left x }
  separate (Right e) = case e of
    Left l  -> { left: Right l, right: Left mempty }
    Right r -> { left: Left mempty, right: Right r }

instance compactableArray :: Compactable Array where
  compact = Array.catMaybes
  separate xs = separateSequence xs

instance compactableList :: Compactable List.List where
  compact = List.catMaybes
  separate xs = separateSequence xs

separateSequence
  :: forall f l r
   . Monoid (f l)
  => Monoid (f r)
  => Foldable f
  => Applicative f
  => Compactable f
  => f (Either l r) -> { left :: f l, right :: f r }
separateSequence = foldl go { left: mempty, right: mempty } where
    go acc e = case e of
      Left l  -> acc { left = acc.left <> pure l }
      Right r -> acc { right = acc.right <> pure r }

instance compactableMap :: Ord k => Compactable (Map.Map k) where
  compact = (Map.fromFoldable :: forall v. Array (Tuple k v) -> Map.Map k v)
            <<< compact
            <<< map sequence
            <<< Map.toUnfoldable

  separate m = { left: Map.fromFoldable sep.left
               , right: Map.fromFoldable sep.right
               }
    where
      sep = separate $ map distributeEither $
            (Map.toUnfoldable :: forall v. Map.Map k v -> Array (Tuple k v)) m
      distributeEither (Tuple k e) = case e of
        Left l  -> Left (Tuple k l)
        Right r -> Right (Tuple k r)

mapMaybe
  :: forall f a b
   . Functor f
  => Compactable f
  => (a -> Maybe b) -> f a -> f b
mapMaybe p = compact <<< map p

mapEither
  :: forall f a l r
   . Functor f
  => Compactable f
  => (a -> Either l r) -> f a -> { left :: f l, right :: f r }
mapEither p = separate <<< map p

applyMaybe
  :: forall f a b
   . Apply f
  => Compactable f
  => f (a -> Maybe b) -> f a -> f b
applyMaybe p = compact <<< apply p

applyEither
  :: forall f a l r
   . Apply f
  => Compactable f
  => f (a -> Either l r) -> f a -> { left :: f l, right :: f r }
applyEither p = separate <<< apply p

bindMaybe
  :: forall m a b
   . Bind m
  => Compactable m
  => m a -> (a -> m (Maybe b)) -> m b
bindMaybe x = compact <<< bind x

bindEither
  :: forall m a l r
   . Bind m
  => Compactable m
  => m a -> (a -> m (Either l r)) -> { left :: m l, right :: m r }
bindEither x = separate <<< bind x

traverseMaybe
  :: forall t m a b
   . Applicative m
  => Traversable t
  => Compactable t
  => (a -> m (Maybe b)) -> t a -> m (t b)
traverseMaybe p = map compact <<< traverse p

traverseEither
  :: forall t m a l r
   . Applicative m
  => Traversable t
  => Compactable t
  => (a -> m (Either l r)) -> t a -> m { left :: t l, right :: t r }
traverseEither p = map separate <<< traverse p
