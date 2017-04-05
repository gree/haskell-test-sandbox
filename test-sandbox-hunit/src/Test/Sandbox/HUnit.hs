-- author: Benjamin Surma <benjamin.surma@gmail.com>

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Test.Sandbox.HUnit (
    assertFailure
  , assertBool
  , assertEqual
  , assertString
  , assertException
  ) where

import Test.Sandbox

import Control.Exception.Lifted

import qualified Test.HUnit

#if MIN_VERSION_HUnit(1,5,0)
import Test.HUnit.Lang (HUnitFailure (..), formatFailureReason)
#else
import Test.HUnit.Lang (HUnitFailure (..))
#endif

-- | Unconditionally signals that a failure has occured.
assertFailure :: String     -- ^ A message that is displayed with the assertion failure
              -> Sandbox ()
assertFailure = wrap . liftIO . Test.HUnit.assertFailure

-- | Asserts that the specified condition holds.
assertBool :: String     -- ^ The message that is displayed if the assertion fails
           -> Bool       -- ^ The condition
           -> Sandbox ()
assertBool s b = wrap $ liftIO (Test.HUnit.assertBool s b)

-- | Asserts that the specified actual value is equal to the expected value.
assertEqual :: (Eq a, Show a)
            => String     -- ^ The message prefix
            -> a          -- ^ The expected value
            -> a          -- ^ The actual value
            -> Sandbox ()
assertEqual s a b = wrap $ liftIO (Test.HUnit.assertEqual s a b)

-- | Signals an assertion failure if a non-empty message (i.e., a message other than "") is passed.
assertString :: String     -- ^ The message that is displayed with the assertion failure
             -> Sandbox ()
assertString s = wrap $ liftIO (Test.HUnit.assertString s)

-- | Signals an assertion failure if *no* exception is raised.
assertException :: String     -- ^ The message that is displayed with the assertion failure
                -> Sandbox a
                -> Sandbox ()
assertException s a =
  assertBool s =<< (a >> return False) `catchError` const (return True)

wrap :: Sandbox () -> Sandbox ()
#if MIN_VERSION_HUnit(1,5,0)
wrap action = action `catch` (\ (HUnitFailure _ e :: HUnitFailure) -> throwError $ formatFailureReason e)
#elif MIN_VERSION_HUnit(1,3,0)
wrap action = action `catch` (\ (HUnitFailure _ e :: HUnitFailure) -> throwError e)
#else
wrap action = action `catch` (\ (HUnitFailure e :: HUnitFailure) -> throwError e)
#endif
