-- author: Benjamin Surma <benjamin.surma@gree.net>

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Framework.Providers.Sandbox.Internals where

import Control.Concurrent
import Control.Monad hiding (fail)
import Data.Either
import Data.Typeable
import Prelude hiding (error, fail)
import qualified Prelude (error)
import System.Console.ANSI
import System.IO

import Test.Framework
import Test.Framework.Providers.API (Testlike (..), TestResultlike (..), runImprovingIO)
import qualified Test.Framework.Providers.API as TF (liftIO)
import Test.Framework.Runners.TestPattern

import Test.Sandbox
import Test.Sandbox.Internals

data SandboxTestResult = Passed
                       | Skipped
                       | Failure String
  deriving (Typeable)

data SandboxTestRunning = Running
  deriving (Typeable)

data SandboxTest = SandboxTest SandboxTestResult
                 | SandboxCleaning (MVar Int)
  deriving (Typeable)

instance Show SandboxTestResult where
  show Passed = "OK"
  show Skipped = "Skipped"
  show (Failure s) = "Failure: " ++ s
instance Show SandboxTestRunning where
  show Running = "Running"

instance TestResultlike SandboxTestRunning SandboxTestResult where
  testSucceeded x = case x of
                      Passed -> True
                      Skipped -> True
                      _ -> False

instance Testlike SandboxTestRunning SandboxTestResult SandboxTest where
  testTypeName _ = "Sandbox tests"
  runTest _ (SandboxTest res) = runImprovingIO $ return res
  runTest _ (SandboxCleaning mvar) = runImprovingIO $ do TF.liftIO $ takeMVar mvar
                                                         return Passed

withTest :: String -> Sandbox b -> Sandbox b
withTest name action = withVariable testVariable name $
  bracket (do level <- getVariable testLevelVariable 0
              liftIO $ printTestName level name
              setVariable testLevelVariable $! level + 1
              return level)
          (setVariable testLevelVariable)
          (const action)

prettyPrintVariable :: String -- Pretty-print variable name
prettyPrintVariable = "__PPRINT__"

testVariable :: String -- Test-list variable name
testVariable = "__TEST__"

testLevelVariable :: String
testLevelVariable = "__TEST_LEVEL__"

indent :: String
indent = "  "

printTestName :: Int -> String -> IO ()
printTestName l t =
  replicateM_ l (putStr indent) >> putStr "[" >> putStrColor Vivid Blue t >> putStr "] " >> hFlush stdout

printTestResult :: Either String a -> IO ()
printTestResult r =
  case r of
    Left error -> putStr " [" >> putStrColor Vivid Red "Fail" >> putStrLn ("] " ++ error)
    _ -> putStr " [" >> putStrColor Vivid Green "OK" >> putStrLn "]"

putStrColor :: ColorIntensity -> Color -> String -> IO ()
putStrColor i c s = do
  setSGR [SetColor Foreground i c]
  putStr s
  setSGR []

-- Wrapper to store the test-framework options
-- for future use by other test-sandbox modules

sandboxSeed :: Maybe Seed -> Maybe SandboxSeed
sandboxSeed s = case s of
  Nothing -> Nothing
  Just (FixedSeed i) -> Just (SandboxFixedSeed i)
  Just RandomSeed -> Just SandboxRandomSeed

sandboxTestOptions :: TestOptions -> SandboxTestOptions
sandboxTestOptions options = SandboxTestOptions (sandboxSeed $ topt_seed options)
                                                (topt_maximum_generated_tests options)
                                                (topt_maximum_unsuitable_generated_tests options)
                                                (topt_maximum_test_size options)

putOptions :: Either String (RunnerOptions, [String]) -> Sandbox ()
putOptions =
  either (const $ return ())
         (\r -> maybe (return ()) (void . Test.Sandbox.Internals.putOptions . sandboxTestOptions) (ropt_test_options $ fst r))

isExcluded :: Either String (RunnerOptions, [String]) -> String -> Bool
isExcluded input name =
  case input of
    Left _ -> False
    Right (options, _) -> case ropt_test_patterns options of
                            Nothing -> False
                            Just patterns -> not $ any (`testPatternMatches` [name]) patterns
