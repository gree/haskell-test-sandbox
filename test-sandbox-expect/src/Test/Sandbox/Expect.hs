{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Sandbox.Expect where

import Control.Monad.State
import System.IO

import Test.Sandbox hiding (withProcess)
import qualified Test.Sandbox (withProcess)
import Test.Sandbox.HUnit

data ExpectState = ExpectState {
    esProcess :: String
  }

newtype ExpectSandbox a = ExpectSandbox {
    runExpectSandbox :: StateT ExpectState Sandbox a
  } deriving (Monad)

withProcess :: String -> ExpectSandbox () -> Sandbox ()
withProcess process actions =
  Test.Sandbox.withProcess process $
    (evalStateT . runExpectSandbox) actions (ExpectState process)

expect :: String -> ExpectSandbox ()
expect output = ExpectSandbox $ do env <- get
                                   output' <- lift $ readLastCapturedOutput (esProcess env)
                                   lift $ assertEqual output output output'

send :: String -> ExpectSandbox ()
send input = ExpectSandbox $ do env <- get
                                void $ lift $ interactWith (esProcess env) input 0
