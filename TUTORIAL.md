
# Filtering

This module defines type classes and functions for dealing with specific sorts of
data types that can be filtered/partitioned.

These data types are always of kind `* -> *`.  That is, they are type-indexed
types.  For example `Array` is a type-indexed type, as it accepts a type
argument: `Array Int`.  `Array` is also an example of one of these data types
that supports filtering.

Filtering is about removing particular values in a data structure based on some
boolean-predicating function.  Given such a description it's easy to see that
filtering is really a kind of partitioning of some data structure into two
and discarding one of them.  We could trivially implement filter and partition
in terms of each other.  In practice, the `Filterable` type class lets the
implementer decide how to implement both, along with a another couple functions
for fusing a `map` operation whilst filtering or partitioning, with `filterMap`
and `partitionMap`.

The `filter` function has the following type:

```purescript
filter :: forall f a. (Filterable f) => (a -> Boolean) -> f a -> f a
```

The following are a few examples of filtering specific to `Array`:

```purescript
> filter (_ > 5) [1, 5, 6, 4, 10, 2]
= [6, 10]

> filter (_ > 5) [1, 2, 3, 4, 5]
= []

> filter (_ > 5) [6]
= [6]
```

The version of `filter` combined with a `map` operation was `filterMap`:

```purescript
filterMap :: forall f a b. (Filterable f) => (a -> Maybe b) -> f a -> f b
```

