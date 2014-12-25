import Test.Hspec
import Test.Hspec.Sandbox
import Test.Sandbox

main :: IO ()
main = withSandbox $ \ref -> do
  hspec $ do
    describe "Basic Test" $ with ref $ do
      it "interactive Test" $ do
        start =<< register "sed_regex" "sed" [ "-u", "s/a/b/" ] def { psCapture = Just CaptureStdout }
        v <- interactWith "sed_regex" "a\n" 1
        liftIO $ v `shouldBe` "b\n"
