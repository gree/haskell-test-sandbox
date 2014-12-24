{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Hspec.Sandbox (
  with
) where

import Test.Sandbox.Internals
import qualified Test.Hspec.Core.Spec as Hspec
import qualified Test.Hspec as Hspec


with :: SandboxStateRef -> Hspec.SpecWith SandboxStateRef -> Hspec.Spec
with ref = Hspec.before (return ref)

instance Hspec.Example (Sandbox ()) where
  type Arg (Sandbox ()) = SandboxStateRef
  evaluateExample example params action =
    Hspec.evaluateExample
      (action $ flip runSandbox' example)
      params
      ($ ())
