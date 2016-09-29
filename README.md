
Control.FromSum
===============

[![Hackage](https://img.shields.io/hackage/v/from-sum.svg)](https://hackage.haskell.org/package/from-sum) [![Build Status](https://secure.travis-ci.org/cdepillabout/from-sum.svg)](http://travis-ci.org/cdepillabout/from-sum)

This Haskell module exports the `fromEitherM` and `fromMaybeM` convenience
functions.

```haskell
fromMaybeM :: m a -> Maybe a -> m a

fromEitherM :: (e -> m a) -> Either e a -> m a
```

`fromEitherM leftAction eitherValue` is the same as `either leftAction pure
eitherValue`.

`fromMaybeM nothingAction maybeValue` is the same as `maybe nothingAction pure
maybeValue`.

## Usage

```haskell
>>> import Control.FromSum (fromEitherM, fromMaybeM)
>>> fromMaybeM [] $ Just 5
[5]
>>> fromMaybeM [] Nothing
[]
>>> fromEitherM (\s -> [length s]) $ Right 5
[5]
>>> fromEitherM (\s -> [length s]) $ Left "foo"
[3]
```
