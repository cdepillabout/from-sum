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
  , fromEitherOrM
  , fromMaybeM
  , fromMaybeOrM
    -- * Monadic in both return and sum-type value
  , fromEitherMM
  , fromEitherOrMM
  , fromMaybeMM
  , fromMaybeOrMM
    -- * Completely non-monadic functions
  , fromEither
  , fromEitherOr
  , fromMaybe
  , fromMaybeOr
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

-- | A 'flip'ed version of 'fromEitherM'.
--
-- >>> fromEitherOrM (Right 5) $ \s -> [length s]
-- [5]
--
-- This can be nice to use as an error handler.
--
-- >>> fromEitherOrM (Right 5) $ \s -> putStrLn ("error: " ++ s) >> undefined
-- 5
-- >>> fromEitherOrM (Left "foo") $ \s -> putStrLn ("error: " ++ s) >> undefined
-- error: foo
-- ...
fromEitherOrM
  :: Applicative m
  => Either e a -> (e -> m a) -> m a
fromEitherOrM = flip fromEitherM

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

-- | A 'flip'ed version of 'fromMaybeM'.
--
-- >>> fromMaybeOrM (Just 5) []
-- [5]
--
-- This can be nice to use as an error handler.
--
-- >>> fromMaybeOrM (Just 5) $ putStrLn "some error occurred" >> undefined
-- 5
-- >>> fromMaybeOrM (Nothing) $ putStrLn "some error occurred" >> undefined
-- some error occurred
-- ...
fromMaybeOrM
  :: Applicative m
  => Maybe a -> m a -> m a
fromMaybeOrM = flip fromMaybeM

-- | Similar to 'fromEitherM' but the 'Either' argument is also a monadic value.
--
-- >>> fromEitherMM (\s -> [length s]) [Right 5, Right 10]
-- [5,10]
-- >>> fromEitherMM (\s -> [length s]) [Left ("foo" :: String), Right 100]
-- [3,100]
--
-- __NOTE__: I don't particularly like the name of this function.  If you have a
-- suggestion for a better name, please submit a PR or issue.
fromEitherMM
  :: Monad m
  => (e -> m a) -> m (Either e a) -> m a
fromEitherMM eitherAction mEither = fromEitherM eitherAction =<< mEither

-- | A 'flip'ed version of 'fromEitherMM'.
fromEitherOrMM
  :: Monad m
  => m (Either e a) -> (e -> m a) -> m a
fromEitherOrMM = flip fromEitherMM

-- | Similar to 'fromMaybeM' but the 'Maybe' argument is also a monadic value.
--
-- >>> fromMaybeMM [] [Just 6, Just 5]
-- [6,5]
-- >>> fromMaybeMM [] [Just 6, Nothing, Just 7]
-- [6,7]
--
-- __NOTE__: I don't particularly like the name of this function.  If you have a
-- suggestion for a better name, please submit a PR or issue.
fromMaybeMM
  :: Monad m
  => m a -> m (Maybe a) -> m a
fromMaybeMM nothingAction mMaybe = fromMaybeM nothingAction =<< mMaybe

-- | A 'flip'ed version of 'fromMaybeMM'.
fromMaybeOrMM
  :: Monad m
  => m (Maybe a) -> m a -> m a
fromMaybeOrMM = flip fromMaybeMM

-- | Similar to 'fromMaybe'.
--
-- >>> fromEither show $ Left 5
-- "5"
-- >>> fromEither show $ Right "hello"
-- "hello"
fromEither :: (e -> a) -> Either e a -> a
fromEither f (Left e) = f e
fromEither _ (Right a) = a

-- | A 'flip'ed version of 'fromEither'.
fromEitherOr :: Either e a -> (e -> a) -> a
fromEitherOr = flip fromEither

-- | A 'flip'ed version of 'fromMaybe'.
fromMaybeOr :: Maybe a -> a -> a
fromMaybeOr = flip fromMaybe
