{- |
   Module    : Test.Framework.Providers.Sandbox
   Maintainer: Benjamin Surma <benjamin.surma@gree.net>

test-framework interface for test-sandbox
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Framework.Providers.Sandbox (
  -- * Introduction
  -- $introduction

  -- * Usage example
  -- $usage

  -- * Initialization
    sandboxTests
  -- * Test declaration
  , sandboxTest
  , sandboxTestGroup
  , sandboxTestGroup'
  , yieldProgress
  ) where

import Control.Exception.Lifted
import Control.Monad hiding (fail)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Error (runErrorT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Either
import Prelude hiding (error, fail)
import qualified Prelude (error)
import System.Console.ANSI
import System.Environment
import System.Exit
import System.IO
import System.IO.Temp

import Test.Framework
import Test.Framework.Providers.API (Test (..))

import Test.Sandbox
import Test.Sandbox.Internals hiding (putOptions)

import Test.Framework.Providers.Sandbox.Internals

-- | Executes tests in the Sandbox monad.
sandboxTests :: String         -- ^ Name of the sandbox environment
             -> [Sandbox Test] -- ^ Tests to perform
             -> Test
sandboxTests name tests = testGroup name [
    buildTest $ do
      options <- interpretArgs =<< getArgs
      if isExcluded options name then return $ Test name (SandboxTest Skipped)
        else withSystemTempDirectory (name ++ "_") $ \dir -> do
               env <- newSandboxState name dir
               result <- (runReaderT . runErrorT . runSandbox) (putOptions options >> sandboxTestGroup name tests `finally` stopAll) env
               case result of
                 Left error -> return $ Test name (SandboxTest (Failure error))
                 Right x -> return x
  ]

-- | Groups tests in the Sandbox monad.
sandboxTestGroup :: String         -- ^ Test group name
                 -> [Sandbox Test] -- ^ Tests to perform
                 -> Sandbox Test
sandboxTestGroup name tests = withTest name $ do
  liftIO $ putStrLn ""
  liftM (testGroup name) (sequence tests)

-- | Variant of sandboxTestGroup: tests will be skipped if the condition is not verified.
sandboxTestGroup' :: String         -- ^ Test group name
                  -> Sandbox Bool   -- ^ Condition for group to be evaluated
                  -> [Sandbox Test] -- ^ Tests to perform if condition stands
                  -> Sandbox Test
sandboxTestGroup' name condition tests = do
  result <- condition
  if result then
    sandboxTestGroup name tests
    else return $ Test (name ++ " (disabled)") (SandboxTest Skipped)

-- | Creates a test from a Sandbox action.
-- Any exception (or error thrown with throwError) will mark the test as failed.
sandboxTest :: String       -- ^ Test name
            -> Sandbox ()   -- ^ Action to perform
            -> Sandbox Test
sandboxTest name test = withTest name $ do
  res <- do
    ref <- ask
    liftIO $ flip (runReaderT . runErrorT . runSandbox) ref $ test `catches` handlers
  liftIO $ printTestResult res
  case res of
    Left error -> return $ Test name (SandboxTest (Failure error))
    Right _ -> return $ Test name (SandboxTest Passed)
  where handlers = [ Handler exitHandler
                   , Handler interruptHandler
                   , Handler otherHandler ]
        exitHandler :: ExitCode -> Sandbox a
        exitHandler = throw
        interruptHandler :: AsyncException -> Sandbox a
        interruptHandler UserInterrupt = liftIO exitFailure
        interruptHandler e = Sandbox . throwError $ show e
        otherHandler :: SomeException -> Sandbox a
        otherHandler = Sandbox . throwError . show

-- | Displays a progress update during a test.
yieldProgress :: String     -- ^ Text to display
              -> Sandbox ()
yieldProgress p = do
  pl <- getVariable prettyPrintVariable []
  unless (null pl) $ liftIO $ putStr " / "
  setVariable prettyPrintVariable (p : pl)
  liftIO $ putStrColor Dull Blue p >> hFlush stdout

----------------------------------------------------------------------
-- Docs
----------------------------------------------------------------------

{- $introduction

This module interfaces the Test.Sandbox monad with the test-framework
popular Haskell package for a unified test experience.

Tests share the same sandboxed environment: processes started in one
test can be addressed in another. Variables can and should be used
to pass information between test cases.
-}

{- $usage

The following example describes how the "sed" example from
the Test.Sandbox would be crammed into the Test.Framework model.

Initialization of the Sandbox is performed by the @sandboxTests@
function. Tests are then individually declared by @sandboxTest@ and
grouped by @sandboxTestGroup@.

> import Test.Framework
> import Test.Framework.Providers.Sandbox
> import Test.Sandbox
> import Test.Sandbox.HUnit
> 
> setup :: Sandbox ()
> setup = start =<< register "sed_s/a/b/" "sed" [ "-u", "s/a/b/" ] def { psCapture = CaptureStdout }
> 
> main = defaultMain [
>     sandboxTests "sed_tests" [
>         sandboxTest "setup" setup
>       , sandboxTest "sed a->b" $ assertEqual "a->b" "b\n" =<< interactWith "sed_s/a/b/" "a\n" 5
>       , sandboxTest "sed aa->ba" $ assertEqual "aa->ba" "ba\n" =<< interactWith "sed_s/a/b/" "aa\n" 5
>     ]
>   ]
-}
