{-#LANGUAGE ScopedTypeVariables#-}
{- |
   Module    : Test.Sandbox
   Maintainer: Benjamin Surma <benjamin.surma@gmail.com>

Configuration and management of processes in a sandboxed environment for system testing.

This module contains extensive documentation. Please scroll down to the Introduction section to continue reading.
-}
module Test.Sandbox (
  -- * Introduction
  -- $introduction

  -- ** Features
  -- $features

  -- ** History
  -- $history

  -- * Usage examples
  -- $usage

  -- ** Communication via TCP
  -- $usage_tcp

  -- ** Communication via standard I/O
  -- $usage_io

  -- Types
    Sandbox
  , ProcessSettings (..)
  , def
  , Capture (..)

  -- * Initialization
  , sandbox
  , withSandbox

  -- * Calling sandbox on IO
  , runSandbox
  , runSandbox'

  -- * Registering processes
  , register

  -- * Managing sandboxed processes
  , run
  , withProcess
  , start
  , startAll
  , stop
  , stopAll
  , signal
  , silently

  -- * Communication
  , interactWith
  , sendTo
  , readLastCapturedOutput
  , getHandles
  , getPort

  -- * Sandbox state management
  , getBinary
  , setPort
  , getFile
  , setFile
  , getDataDir
  , checkVariable
  , getVariable
  , setVariable
  , unsetVariable
  , withVariable

  -- * Sandbox exception handling
  , bracket
  , catchError
  , finally
  , throwError

  -- * Sandbox I/O handling
  , liftIO
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Error.Class (catchError, throwError)
import qualified Data.ByteString.Char8 as B
import Data.Default
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Data.Serialize (Serialize)
import Prelude hiding (error)
import System.Exit
import System.IO
import System.IO.Temp
import System.Posix hiding (release)
import System.Environment

import Test.Sandbox.Internals

cleanUp :: Sandbox ()
cleanUp = do
  stopAll
  whenM isCleanUp $ do
    cleanUpProcesses

-- | Creates a sandbox and execute the given actions in the IO monad.
sandbox :: String    -- ^ Name of the sandbox environment
        -> Sandbox a -- ^ Action to perform
        -> IO a
sandbox name actions = withSystemTempDirectory (name ++ "_") $ \dir -> do
  env <- newSandboxState name dir
  runSandbox (actions `finally` cleanUp) env >>= either
    (\error -> do hPutStrLn stderr error
                  throwIO $ userError error)
    return

withSandbox :: (SandboxStateRef -> IO a) -> IO a
withSandbox actions = do
  name <- getProgName
  sandbox name $ do
    ref <- ask
    liftIO $ actions ref

-- | Optional parameters when registering a process in the Sandbox monad.
data ProcessSettings =
  ProcessSettings {
    psWait :: Maybe Int        -- ^ Time to wait (in s.) before checking that the process is still up
  , psCapture :: Maybe Capture -- ^ Which outputs to capture (if any)
  } |
  ProcessSettings2 {
    psWait :: Maybe Int              -- ^ Time to wait (in s.) before checking that the process is still up
  , psCapture :: Maybe Capture       -- ^ Which outputs to capture (if any)
  , psEnv :: Maybe [(String,String)] -- ^ Environment variables
  , psCwd :: Maybe FilePath          -- ^ Working directory for the new process
  }

instance Default ProcessSettings where
  def = ProcessSettings2 (Just 1) Nothing Nothing Nothing

-- | Registers a process in the Sandbox monad.
register :: String          -- ^ Process name
         -> FilePath        -- ^ Path to the application binary
         -> [String]        -- ^ Arguments to pass on the command-line
         -> ProcessSettings -- ^ Process settings
         -> Sandbox String
register name bin args (ProcessSettings wait capture) =
  registerProcess name bin args wait capture Nothing Nothing >> return name
register name bin args (ProcessSettings2 wait capture env cwd) =
  registerProcess name bin args wait capture env cwd >> return name

-- | Communicates with a sandboxed process via TCP and returns the answered message as a string.
sendTo :: String         -- ^ Name of the registered port 
       -> String         -- ^ Input string
       -> Int            -- ^ Time to wait before timeout (in milli-seconds)
       -> Sandbox String
sendTo = sendToPort

-- | Starts the given process, runs the action, then stops the process.
-- The process is managed by the functions start and stop respectively.
withProcess :: String    -- ^ Process name
            -> Sandbox a -- ^ Action to run
            -> Sandbox a
withProcess name action = bracket (start name) (const $ stop name) (const action)

-- | Helper function: starts a process, wait for it to terminate and return its captured output.
run :: String -> Int -> Sandbox (ExitCode, Maybe String)
run name timeout = do
  silently $ start name
  waitFor name timeout `catchError` (\e -> silently (stop name) >> throwError e)

-- | Starts a previously registered process (verbose)
start :: String     -- ^ Process name
      -> Sandbox ()
start process = uninterruptibleMask_ $ do
  installSignalHandlers
  displayBanner
  sp <- getProcess process
  whenM isVerbose $ liftIO $ putStr ("Starting process " ++ process ++ "... ") >> hFlush stdout
  _ <- updateProcess =<< startProcess sp
  whenM isVerbose $ liftIO $ putStrLn "Done."

-- | Starts all registered processes (in their registration order)
startAll :: Sandbox ()
startAll = uninterruptibleMask_ $ do
  displayBanner
  whenM isVerbose $ liftIO $ putStrLn "Starting all sandbox processes... " >> hFlush stdout
  silently $ do env <- get
                mapM_ start (ssProcessOrder env)
  whenM isVerbose $ liftIO $ putStrLn "Done."

waitFor :: String -> Int -> Sandbox (ExitCode, Maybe String)
waitFor name timeout = waitFor' 0
  where waitFor' tick = do
          sp <- getProcess name
          case spInstance sp of
            Just (StoppedInstance ec o) -> return (ec, o)
            _ -> if tick > timeout then throwError $ "Process " ++ name ++ " still running after " ++ show timeout ++ "s timeout."
                   else do liftIO $ threadDelay secondInµs
                           waitFor' $! tick + 1

-- | Gracefully stops a previously started process (verbose)
stop :: String     -- ^ Process name
     -> Sandbox ()
stop process = uninterruptibleMask_ $ do
  sp <- getProcess process
  whenM isVerbose $ liftIO $ putStrLn ("Stopping process " ++ process ++ "("++ show (spPid sp) ++ ")... ") >> hFlush stdout
  _ <- updateProcess =<< stopProcess sp
  whenM isVerbose $ liftIO $ putStrLn "Done." >> hFlush stdout

-- | Sends a POSIX signal to a process
signal :: String     -- ^ Process name
       -> Signal     -- ^ Signal to send
       -> Sandbox ()
signal process sig = uninterruptibleMask_ $ do
  sp <- getProcess process
  case spInstance sp of
    Just (RunningInstance ph _ _ _) -> liftIO $ hSignalProcess sig ph
    _ -> throwError $ "Process " ++ process ++ " is not running."

-- | Gracefully stops all registered processes (in their reverse registration order)
stopAll :: Sandbox ()
stopAll = uninterruptibleMask_ $ do
  whenM isVerbose $ liftIO $ putStr "Stopping all sandbox processes... " >> hFlush stdout
  env <- get
  mapM_ stop (reverse $ ssProcessOrder env)
  whenM isVerbose $ liftIO $ putStrLn "Done."

-- | Returns the effective binary path of a registered process.
getBinary :: String           -- ^ Process name
          -> Sandbox FilePath
getBinary process = getProcess process >>= getProcessBinary

-- | Returns the handles used to communicate with a registered process using standard I/O.
getHandles :: String                   -- ^ Process name
           -> Sandbox (Handle, Handle)
getHandles process = do
  sp <- getProcess process
  input <- getProcessInputHandle sp
  output <- getProcessCapturedOutputHandle sp
  return (input, output)

-- | Returns the last captured output of a started process.
readLastCapturedOutput :: String         -- ^ Process name
                       -> Sandbox String
readLastCapturedOutput process = do
  sp <- getProcess process
  h <- getProcessCapturedOutputHandle sp
  b <- hReadWithTimeout h 0
  return $! B.unpack b

-- | Interacts with a sandboxed process via standard I/O.
interactWith :: String         -- ^ Process name
             -> String         -- ^ Input string
             -> Int            -- ^ Time to wait before timeout (in milli-seconds)
             -> Sandbox String
interactWith process input timeout = do
  sp <- getProcess process
  interactWithProcess sp input timeout

-- | Returns an unbound user TCP port and stores it for future reference.
getPort :: String             -- ^ Port name for future reference
        -> Sandbox Port
getPort name = do
  env <- get
  case M.lookup name $ ssAllocatedPorts env of
    Just port -> return port
    Nothing -> getNewPort name

-- | Explicitely sets a port to be returned by getPort.
setPort :: String             -- ^ Port name for future reference
        -> Int                -- ^ TCP port number
        -> Sandbox Port
setPort name port = do
  let port' = fromIntegral port
  bindable <- liftIO $ isBindable (fromIntegral port)
  if bindable then do env <- get
                      _ <- put (env { ssAllocatedPorts = M.insert name port' $ ssAllocatedPorts env })
                      return port'
    else throwError $ "Unable to bind port " ++ show port

-- | Creates a temporary file in the sandbox and returns its path.
setFile :: String           -- ^ File name for future reference
        -> String           -- ^ File contents
        -> Sandbox FilePath
setFile name contents = do
  env <- get
  (file, env') <- liftIO $ setFile' name contents env
  _ <- put env'
  return file

-- | Returns the path of a file previously created by setFile.
getFile :: String           -- ^ File name used during setFile
        -> Sandbox FilePath
getFile name = do
  env <- get
  case M.lookup name $ ssFiles env of
    Just file -> return file
    Nothing -> throwError $ "Config file " ++ name ++ " does not exist."

-- | Temporarily sets a variable for the execution of the given action.
withVariable :: (Serialize a)
             => String    -- ^ Variable key
             -> a         -- ^ Variable value
             -> Sandbox b -- ^ Action to run
             -> Sandbox b
withVariable key value action = bracket (do env <- get
                                            let old = M.lookup key $ ssVariables env
                                            _ <- setVariable key value
                                            return old)
                                        (\old -> case old of
                                                   Nothing -> unsetVariable key
                                                   Just old' -> void $ setVariable key old')
                                        (const action)

-- | Returns the temporary directory used to host the sandbox environment.
getDataDir :: Sandbox FilePath
getDataDir = liftM ssDataDir get

-- | Executes the given action silently.
silently :: Sandbox a -- ^ Action to execute
       -> Sandbox a
silently = withVariable verbosityKey False

----------------------------------------------------------------------
-- Docs
----------------------------------------------------------------------

{- $introduction

test-sandbox is a framework to manage external applications
and communicate with them via TCP or standard I/O for system testing
in a sandboxed environment. The Test.Sandbox monad can either be used
stand-alone or in conjunction with HUnit, QuickCheck and the
test-framework packages to build a complete test suite.

The API is meant to be simple to understand yet flexible enough
to meet most of the needs of application testers.
-}

{- $features

 * Register, start and stop programs in a sandboxed environment.

 * Automatic cleaning at shutdown: started processes are shutdown,
   temporary files are deleted.

 * Ask the framework to provide you with random,
   guaranteed not bound TCP ports for your tests:
   no more collisions when running 2 sets of tests at the same time.

 * Generate your temporary configuration files programatically
   in a secure manner.

 * Easily share variables between your tests and modify them
   at runtime.

 * Combine with the test-framework package for standardized output
   and XML test result generation.

 * Use the QuickCheck library to write property tests and generate
   automatic test cases for your external application;
   enjoy the full power of the Haskell test harness, even if
   the application to test is written in a different language!
-}

{- $history

At GREE, we spend lots of time meticulously testing
our internally-developed middleware.
We have solutions not only developed in Haskell, but also C++
and PHP, but wanted a simple and robust test framework to perform
end-to-end testing, and this is how test-sandbox is born.
-}

{- $usage

A basic test-sandbox usecase would be as follows:

 1. Initialize a Test.Sandbox monad

 2. Register one or several processes to test
    a. Ask the Sandbox to provide you with some free TCP ports
       if needed
    a. Prepare temporary configuration files if required
       by your application

 3. Start some processes

 4. Communicate with them via TCP or standard IO

 5. Analyze the received answers and check whether they match
    an expected pattern

 6. Error handling is done via the @throwError@ and @catchError@
    functions.

Once all tests are done, the started processes are automatically
killed, and all temporary files are deleted.
-}

{- $usage_tcp

The following example shows a simple test for the "memcached"
NoSQL key-value store.

First, the sandbox is initialized with the @sandbox@ function;
then, it is asked to provide a free TCP port, which will be used
by the memcached process.
Once the program is registered with the @register@ function,
it is started with the @start@ function.
Please note that the Sandbox monad keeps an internal state: processes
registered in a function can be referenced in another without issues.

Communication via TCP is performed with the @sendTo@ function:
its arguments are the port name (given at the time of @getPort@),
the input string, and a timeout in milli-seconds. The function
returns the received TCP answer, if one was received in the correct
timeframe, or fails by throwing an error (which can be caught by
@catchError@).

The test is performed with the @assertEqual@ function from the HUnit
package. In case of matching failure, it will throw an exception,
which, if uncaught (like it is) will cause the Sandbox to perform
cleaning and rethrow the exception.

> import Test.Sandbox
> import Test.Sandbox.HUnit
> 
> setup :: Sandbox ()
> setup = do
>   port <- getPort "memcached"
>   register "memcached" "memcached" [ "-p", show port ] def
> 
> main :: IO ()
> main = sandbox $ do
>   setup
>   start "memcached"
>   output <- sendTo "memcached" "set key 0 0 5\r\nvalue\r\n" 1
>   assertEqual "item is stored" "STORED\r\n" output
-}

{- $usage_io
The next example is a hypothetic system test for the popular "sed",
the popular Unix stream editor.

Please note that at registration time, the @psCapture@ parameter is
set to @CaptureStdout@. This is required by the @interactWith@
function, used for communication on the standard input, which will
return the captured output on each request.

> import Test.Sandbox
> import Test.Sandbox.HUnit
> 
> main :: IO ()
> main = sandbox $ do
>   start =<< register "sed_regex" "sed" [ "-u", "s/a/b/" ] def { psCapture = CaptureStdout }
>   assertEqual "a->b" "b\n" =<< interactWith "sed_regex_ "a\n" 5
-}
