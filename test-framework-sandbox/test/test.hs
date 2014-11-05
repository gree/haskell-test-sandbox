import Test.Framework.Providers.Sandbox (sandboxTests, sandboxTest, sandboxTestGroup, yieldProgress, withRetry)
import Test.Sandbox
import Test.Framework
import Test.Sandbox.HUnit (assertEqual)
import Data.IORef

main :: IO ()
main = defaultMain [ allCases ]
      

retryTest1 :: Sandbox Test
retryTest1 = sandboxTestGroup "retryTest(inside of testGroup)" [
  sandboxTest "retry" $ do
    val <- liftIO $ newIORef 0
    withRetry 10 $ do
      val' <- liftIO $ readIORef val
      liftIO $ writeIORef val (val'+1)
      assertEqual "retry 10(single)" 9 val'
    withRetry 2 $ do
      withRetry 3 $ do
        val' <- liftIO $ readIORef val
        liftIO $ writeIORef val (val'+1)
        assertEqual "retry 6(double loop)" 15 val'
  ]
              

retryTest2 :: Sandbox Test
retryTest2 = do
  val <- liftIO $ newIORef 0
  val2 <- liftIO $ newIORef 0
  withRetry 10 $
    sandboxTestGroup "retryTest(outside of testGroup)" [
      sandboxTest "retry" $ do
        val' <- liftIO $ readIORef val
        liftIO $ writeIORef val (val'+1)
        assertEqual "retry 10(single)" 9 val'
    , sandboxTest "retry2" $ do
        val2' <- liftIO $ readIORef val2
        liftIO $ writeIORef val2 (val2'+1)
        assertEqual "retry 10(single)" 9 val2'
    ]
              

allCases :: Test
allCases = sandboxTests "framework-test" [
       retryTest1
     , retryTest2
   ]

