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
  , fromEitherM_
  , fromEitherOrM_
  , fromMaybeM
  , fromMaybeOrM
  , fromMaybeM_
  , fromMaybeOrM_
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
    -- * Converting from 'Maybe' to 'Either'
  , maybeToEither
  , maybeToEitherOr
  , eitherToMaybe
    -- * Collapsing funtions
  , collapseEither
  , collapseExceptT
  ) where

#if __GLASGOW_HASKELL__ < 710
-- We don't need this import for GHC 7.10 as it exports all required functions
-- from Prelude
import Control.Applicative
#endif
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
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

-- | Similar to 'fromEitherM', but only run the monadic 'leftAction' if the
-- 'Either' argument is 'Left'.  Otherwise, return 'pure' 'mempty'.
--
-- @
--  'fromEitherM_' leftAction === 'either' leftAction ('const' '$' 'pure' 'mempty')
-- @
--
-- >>> fromEitherM_ (\err -> putStrLn err >> pure "bye") $ Right 5
-- ""
-- >>> fromEitherM_ (\err -> putStrLn err >> pure "bye") $ Left "there was an error"
-- there was an error
-- "bye"
--
-- This can be convenient when you want to run some sort of logging function
-- whenever an 'Either' is 'Left'.  If you imagine the logging function is
-- @b -> 'IO' '()'@, then the effective type of 'fromEitherM_' becomes
-- @'fromEitherM_' :: (e -> 'IO' '()') -> 'Either' e a -> 'IO' '()'@, because
-- '()' has a 'Monoid' instance, and 'IO', has an 'Applicative' instance.
--
-- >>> fromEitherM_ putStrLn $ Left "there was an error"
-- there was an error
fromEitherM_
  :: (Applicative m, Monoid b)
  => (e -> m b) -> Either e a -> m b
fromEitherM_ leftAction = either leftAction (const $ pure mempty)

-- | A 'flip'ed version of 'fromEitherM_'.
fromEitherOrM_
  :: (Applicative m, Monoid b)
  => Either e a -> (e -> m b) -> m b
fromEitherOrM_ = flip fromEitherM_

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

-- | Similar to 'fromMaybeM', but only run the monadic 'nothingAction' if the
-- 'Maybe' argument is 'Nothing'.  Otherwise, return 'pure' 'mempty'.
--
-- @
--  'fromMaybeM_' nothingAction === 'maybe' nothingAction ('const' '$' 'pure' 'mempty')
-- @
--
-- >>> fromMaybeM_ (putStrLn "hello" >> pure "bye") $ Just 5
-- ""
-- >>> fromMaybeM_ (putStrLn "hello" >> pure "bye") Nothing
-- hello
-- "bye"
--
-- This can be convenient when you want to run some sort of logging function
-- whenever a 'Maybe' is 'Nothing'.  If you imagine the logging function is
-- @'IO' '()'@, then the effective type of 'fromMaybeM_' becomes
-- @'fromMaybeM_' :: 'IO' '()' -> 'Maybe' a -> 'IO' '()'@, because '()' has a
-- 'Monoid' instance, and 'IO', has an 'Applicative' instance.
--
-- >>> fromMaybeM_ (putStrLn "hello") Nothing
-- hello
fromMaybeM_
  :: (Applicative m, Monoid b)
  => m b -> Maybe a -> m b
fromMaybeM_ nothingAction = maybe nothingAction (const $ pure mempty)

-- | A 'flip'ed version of 'fromMaybeM'.
fromMaybeOrM_
  :: (Applicative m, Monoid b)
  => Maybe a -> m b -> m b
fromMaybeOrM_ = flip fromMaybeM_

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
fromEither f = either f id

-- | A 'flip'ed version of 'fromEither'.
fromEitherOr :: Either e a -> (e -> a) -> a
fromEitherOr = flip fromEither

-- | A 'flip'ed version of 'fromMaybe'.
fromMaybeOr :: Maybe a -> a -> a
fromMaybeOr = flip fromMaybe

-- | Collapse an @'Either' a a@ to an @a@.  Defined as @'fromEither' 'id'@.
--
-- Note: Other libraries export this function as @fromEither@, but our
-- 'fromEither' function is slightly more general.
--
-- >>> collapseEither (Right 3)
-- 3
-- >>> collapseEither (Left "hello")
-- "hello"
collapseEither :: Either a a -> a
collapseEither = fromEither id

-- | Similar to 'collapseEither', but for 'ExceptT'.
--
-- >>> import Control.Monad.Trans.Except (ExceptT(ExceptT))
-- >>> collapseExceptT (ExceptT $ pure (Right 3))
-- 3
-- >>> collapseExceptT (ExceptT $ pure (Left "hello"))
-- "hello"
collapseExceptT :: Monad m => ExceptT a m a -> m a
collapseExceptT = pure . collapseEither <=< runExceptT

-- | Convert a 'Maybe' to an 'Either'.
--
-- If the 'Maybe' is 'Just', then return the value in 'Right'.
--
-- >>> maybeToEither 3 $ Just "hello"
-- Right "hello"
--
-- If the 'Maybe' is 'Nothing', then use the given @e@ as 'Left'.
--
-- >>> maybeToEither 3 Nothing
-- Left 3
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

-- | A 'flip'ed version of 'maybeToEither'.
--
-- >>> maybeToEitherOr (Just "hello") 3
-- Right "hello"
--
-- >>> maybeToEitherOr Nothing 3
-- Left 3
maybeToEitherOr :: Maybe a -> e -> Either e a
maybeToEitherOr = flip maybeToEither

-- | Convert an 'Either' to a 'Maybe'.
--
-- A 'Right' value becomes 'Just'.
--
-- >>> eitherToMaybe $ Right 3
-- Just 3
--
-- A 'Left' value becomes 'Nothing'.
--
-- >>> eitherToMaybe $ Left "bye"
-- Nothing
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a
