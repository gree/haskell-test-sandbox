{-# LANGUAGE OverloadedStrings #-}

import Test.Sandbox
import Test.Hspec
import System.Process


main=do
  withSandbox $ \env -> do
    runSB env $ do
      registerCmd "flare"
        (readProcessWithExitCode "docker" ["run" "-d" "-p" "12121:12121" "junjih/flare-docker"] "")
        (\(_,id,_) -> readProcessWithExitCode "docker" ["rm" "-f" id] "")
    return ()
    hspec $ do
      describe "test-sandbox" $ do
        it "hgoehoge" $ do
          True `shouldBe` True
      


