-- author: Benjamin Surma <benjamin.surma@gmail.com>

{-# LANGUAGE CPP #-}

module Test.Sandbox.QuickCheck (
    quickCheck
  , quickCheckWith
  , verboseCheck
  , verboseCheckWith
  ) where

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Error (runErrorT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)
import System.Random

#if !MIN_VERSION_QuickCheck(2,6,0)
import Data.List (isInfixOf)
#endif

import Test.Sandbox.Internals

import Test.QuickCheck hiding (quickCheck, quickCheckWith, verboseCheck, verboseCheckWith)
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property hiding (Result, interrupted, reason, theException)
#if MIN_VERSION_QuickCheck(2,7,0)
import Test.QuickCheck.Random
import Test.QuickCheck.Exception
#endif


-- | Tests a property and prints the results to stdout.
quickCheck :: PropertyM Sandbox () -> Sandbox ()
quickCheck prop = getQuickCheckOptions
  >>= maybe (quickCheck' quickCheckResult prop) (`quickCheckWith` prop)

-- | Tests a property, using test arguments, and prints the results to stdout.
quickCheckWith :: Args -> PropertyM Sandbox () -> Sandbox ()
quickCheckWith args = quickCheck' (quickCheckWithResult args)

-- | Tests a property and prints the results and all test cases generated to stdout.
verboseCheck :: PropertyM Sandbox () -> Sandbox ()
verboseCheck prop = getQuickCheckOptions
  >>= maybe (quickCheck' verboseCheckResult prop) (`verboseCheckWith` prop)

-- | Tests a property, using test arguments, and prints the results and all test cases generated to stdout.
verboseCheckWith :: Args -> PropertyM Sandbox () -> Sandbox ()
verboseCheckWith args = quickCheck' (verboseCheckWithResult args)

quickCheck' :: (Property -> IO Result) -> PropertyM Sandbox () -> Sandbox ()
quickCheck' tester prop = do
  seed <- getVariable seedVariable Nothing :: Sandbox (Maybe Int)
  ref <- ask
  res <- liftIO . tester $ monadic (runSandboxProperty ref) prop
  case res of
#if MIN_VERSION_QuickCheck(2,7,0)
    Failure { theException = e, output = o } -> if fmap isInterrupt e == Just True then liftIO exitFailure
                                                  else throwError (o ++ maybe "" (\s -> " (used seed " ++ show s ++ ")") seed)
#elif MIN_VERSION_QuickCheck(2,6,0)
    Failure { interrupted = i, output = o } -> if i then liftIO exitFailure
                                                 else throwError (o ++ maybe "" (\s -> " (used seed " ++ show s ++ ")") seed)
#else
    Failure { reason = r, output = o } -> if "user interrupt" `isInfixOf` r then liftIO exitFailure
                                            else throwError (o ++ maybe "" (\s -> " (used seed " ++ show s ++ ")") seed)
#endif
    NoExpectedFailure { output = o } -> throwError o
    _ -> return ()

runSandboxProperty :: SandboxStateRef -> Sandbox Property -> Property
runSandboxProperty ref prop = morallyDubiousIOProperty $
  (runReaderT . runErrorT . runSandbox) prop ref >>= either error return

getQuickCheckOptions :: Sandbox (Maybe Args)
getQuickCheckOptions = do
  options <- getOptions
  case options of
    Nothing -> do
      (gen, seed) <- randomSeed
      setVariable seedVariable (Just seed)
      return $ Just stdArgs { replay = Just (gen, 0) }
    Just stuff -> do
      (gen, seed) <- case stoSeed stuff of
                       Nothing -> randomSeed
                       Just SandboxRandomSeed -> randomSeed
                       Just (SandboxFixedSeed i) -> fixedSeed i
      setVariable seedVariable (Just seed)
      return $ Just stdArgs { replay = Just (gen, 0)
                            , maxSuccess = fromMaybe (maxSuccess stdArgs) (stoMaximumGeneratedTests stuff)
                            , maxDiscardRatio = fromMaybe (maxDiscardRatio stdArgs) (stoMaximumUnsuitableGeneratedTests stuff)
                            , maxSize = fromMaybe (maxSize stdArgs) (stoMaximumTestSize stuff) }
  where randomSeed = liftIO randomIO >>= fixedSeed
#if MIN_VERSION_QuickCheck(2,7,0)
        fixedSeed s = return (mkQCGen s, s)
#else
        fixedSeed s = return (mkStdGen s, s)
#endif

seedVariable :: String
seedVariable = "__QUICKCHECK_SEED__"
