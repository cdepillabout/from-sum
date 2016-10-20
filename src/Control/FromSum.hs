{-# LANGUAGE CPP #-}

{-|
Module      : Control.FromSum
Copyright   : (c) Dennis Gosnell, 2016
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

This Haskell module exports various \"from\" functions for 'Either' and
'Maybe'.
-}
module Control.FromSum
  ( -- * Monadic in return value
    fromEitherM
  , fromMaybeM
    -- * Monadic in both return and sum-type value
  , fromEitherMM
  , fromMaybeMM
    -- * Completely non-monadic functions
  , fromEither
  , fromMaybe
  ) where
#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif
import Data.Maybe (fromMaybe)

-- | A monadic version of 'fromEither'.
--
-- @
--  'fromEitherM' leftAction === 'either' leftAction 'pure'
-- @
--
-- >>> fromEitherM (\s -> [length s]) $ Right 5
-- [5]
-- >>> fromEitherM (\s -> [length s]) $ Left ("foo" :: String)
-- [3]
fromEitherM
  :: Applicative m
  => (e -> m a) -> Either e a -> m a
fromEitherM leftAction = either leftAction pure

-- | A monadic version of 'fromMaybe'.
--
-- @
--  'fromMaybeM' nothingAction === 'maybe' nothingAction 'pure'
-- @
--
-- >>> fromMaybeM [] $ Just 5
-- [5]
-- >>> fromMaybeM [] Nothing
-- []
fromMaybeM
  :: Applicative m
  => m a -> Maybe a -> m a
fromMaybeM nothingAction = maybe nothingAction pure

-- | Similar to 'fromEitherM' but 'Either' argument is also a monadic value.
--
-- >>> fromEitherMM (\s -> [length s]) . pure $ Right 5
-- [5]
-- >>> fromEitherMM (\s -> [length s]) . pure $ Left ("foo" :: String)
-- [3]
--
-- __NOTE__: I don't particularly like the name of this function.  If you have a
-- suggestion for a better name, please submit a PR or issue.
fromEitherMM
  :: Monad m
  => (e -> m a) -> m (Either e a) -> m a
fromEitherMM eitherAction mEither = fromEitherM eitherAction =<< mEither

-- | Similar to 'fromMaybeMA monadic version of 'fromMaybe'.
--
-- >>> fromMaybeMM [] . pure $ Just 5
-- [5]
-- >>> fromMaybeMM [] $ pure Nothing
-- []
--
-- __NOTE__: I don't particularly like the name of this function.  If you have a
-- suggestion for a better name, please submit a PR or issue.
fromMaybeMM
  :: Monad m
  => m a -> m (Maybe a) -> m a
fromMaybeMM nothingAction mMaybe = fromMaybeM nothingAction =<< mMaybe

-- | Similar to 'fromMaybe'.
--
-- >>> fromEither show $ Left 5
-- "5"
-- >>> fromEither show $ Right "hello"
-- "hello"
fromEither :: (e -> a) -> Either e a -> a
fromEither f (Left e) = f e
fromEither _ (Right a) = a
