-- author: Benjamin Surma <benjamin.surma@gmail.com>

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Sandbox.Internals where

import Control.Applicative (Applicative)
import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Base (MonadBase)
import Control.Monad.Error (MonadError, catchError, throwError)
import Control.Monad.Loops
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Error (ErrorT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Serialize (Serialize, decode, encode)
import GHC.Generics (Generic)
import GHC.IO.Handle
import Network
import Network.Socket
import Prelude hiding (error)
import qualified Prelude (error)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error (isEOFError, tryIOError)
import System.Posix hiding (killProcess)
import System.Process hiding (env, waitForProcess)
import System.Process.Internals (withProcessHandle, ProcessHandle__(OpenHandle))
import System.Random
import System.Random.Shuffle

type SandboxStateRef = IORef SandboxState

newtype Sandbox a = Sandbox {
    runSandbox :: ErrorT String (ReaderT SandboxStateRef IO) a
  } deriving (Applicative, Functor, Monad, MonadBase IO, MonadError String, MonadReader (IORef SandboxState), MonadIO)

instance MonadBaseControl IO Sandbox where
  newtype StM Sandbox a = StMSandbox { runStMSandbox :: StM (ErrorT String (ReaderT SandboxStateRef IO)) a }
  liftBaseWith f = Sandbox . liftBaseWith $ \run -> f (liftM StMSandbox . run . runSandbox)
  restoreM = Sandbox . restoreM . runStMSandbox

data SandboxState = SandboxState {
    ssName :: String
  , ssDataDir :: FilePath
  , ssProcesses :: Map String SandboxedProcess
  , ssProcessOrder :: [String]
  , ssAllocatedPorts :: Map String PortNumber
  , ssAvailablePorts :: [PortNumber]
  , ssFiles :: Map String FilePath
  , ssVariables :: Map String ByteString
  }

data SandboxedProcess = SandboxedProcess {
    spName :: String
  , spBinary :: FilePath
  , spArgs :: [String]
  , spWait :: Maybe Int
  , spCapture :: Maybe Capture
  , spInstance :: Maybe SandboxedProcessInstance
  }

data Capture = CaptureStdout
               | CaptureStderr
               | CaptureBoth

data SandboxedProcessInstance = RunningInstance ProcessHandle Handle (Maybe Handle)
                              | StoppedInstance ExitCode (Maybe String)

get :: Sandbox SandboxState
get = ask >>= liftIO . readIORef

put :: SandboxState -> Sandbox SandboxState
put state = do
  ref <- ask
  liftIO $ writeIORef ref state
  return state

pretty :: SandboxState -> String
pretty env =
  header env ++
  "-- Data directory: " ++ ssDataDir env ++ "\n\
  \-- Allocated ports: " ++ unwords (map show $ M.assocs $ ssAllocatedPorts env) ++ "\n\
  \-- Configuration files: " ++ unwords (M.elems $ ssFiles env) ++ "\n\
  \-- Registered processes: " ++ unwords (ssProcessOrder env)
  ++ footer

header :: SandboxState -> String
header te =
  "\n\
  \##------------------------------------------------------------------------------\n\
  \ ## " ++ title ++ replicate (72 - length title) ' ' ++ "  --\n\
  \## ##---------------------------------------------------------------------------\n"
  where title = ssName te ++ " system test environment"

footer :: String
footer =
  "\n--------------------------------------------------------------------------------\n"

newSandboxState :: String -> FilePath -> IO SandboxStateRef
newSandboxState name dir = do
  gen <- newStdGen
  let availablePorts = shuffle' userPorts (length userPorts) gen
                       where userPorts = [49152..65535]
  newIORef $ SandboxState name dir M.empty [] M.empty availablePorts M.empty M.empty

registerProcess ::
  String -> FilePath -> [String] -> Maybe Int -> Maybe Capture
  -> Sandbox SandboxedProcess
registerProcess name bin args wait capture = do
  -- Validate process name
  unless (isValidProcessName name) $
    throwError $ "Invalid process name: " ++ name ++ "."
  -- Register into the environment
  env <- get
  if isJust (M.lookup name (ssProcesses env)) then
    throwError $ "Process " ++ name ++ " is already registered in the test environment."
    else do let sp = SandboxedProcess name bin args wait capture Nothing
            put env { ssProcesses = M.insert name sp (ssProcesses env)
                    , ssProcessOrder = ssProcessOrder env ++ [name] }
            return sp

isValidProcessName :: String -> Bool
isValidProcessName s = not (null s)
    && isAlpha (head s)
    && all isAllowed (tail s)
  where isAllowed c = isAlphaNum c || c == '_'

getProcess :: String -> Sandbox SandboxedProcess
getProcess name = do
  env <- get
  case M.lookup name (ssProcesses env) of
    Just sp -> let spi = spInstance sp in
      case spi of
        Just (RunningInstance ph _ oh) -> do
          ec <- liftIO $ getProcessExitCode ph
          case ec of
            Just ec' -> do -- Process is dead; update the environment
              o <- case oh of
                     Just oh' -> liftM Just $ liftIO $ hGetContents oh'
                     Nothing -> return Nothing
              let sp' = sp { spInstance = Just $ StoppedInstance ec' o }
              updateProcess sp'
              return sp'
            Nothing -> return sp
        _ -> return sp
    _ -> throwError $ "Process " ++ name ++ " is not registered in the test environment."

updateProcess :: SandboxedProcess -> Sandbox SandboxedProcess
updateProcess sp = do
  env <- get
  put env { ssProcesses = M.insert (spName sp) sp (ssProcesses env) }
  return sp

secondInµs :: Int
secondInµs = 1000000

setFile' :: String -> String -> SandboxState -> IO (FilePath, SandboxState)
setFile' name contents env = do
  (f, h) <- openTempFile (ssDataDir env) name
  hPutStr h contents
  hClose h
  return (f, env { ssFiles = M.insert name f (ssFiles env) })

bufferSize :: Int
bufferSize = 4096

hReadWithTimeout :: Handle -> Int -> Sandbox ByteString
hReadWithTimeout h timeout = do
  dataAvailable <- liftIO $ hWaitForInput h timeout `catch` checkEOF
  if dataAvailable then do b <- liftIO $ B.hGetNonBlocking h bufferSize
                           b' <- hReadWithTimeout h timeout `catchError` (\_ -> return $ B.pack [])
                           return $ B.append b b' -- TODO: Rewrite as terminal recursive
    else throwError $ "No data after " ++ show timeout ++ "ms timeout."
  where
    checkEOF :: IOError -> IO Bool
    checkEOF e = if isEOFError e then do threadDelay $ timeout * 1000
                                         liftM not $ hIsEOF h
                   else return True

sendToPort :: String -> String -> Int -> Sandbox String
sendToPort name input timeout = do
  env <- get
  case M.lookup name (ssAllocatedPorts env) of
    Nothing -> throwError $ "No such allocated port: " ++ name
    Just port -> do h <- liftIO . withSocketsDo $ connectTo "localhost" $ PortNumber port
                    liftIO $ do B.hPutStr h $ B.pack input
                                hFlush h
                    b <- hReadWithTimeout h timeout
                    liftIO $ hClose h
                    return $! B.unpack b

getNewPort :: String -> Sandbox PortNumber
getNewPort name = do
  env <- get
  case ssAvailablePorts env of
    [] -> throwError "No user ports left."
    ports -> do (port, ports') <- liftIO $ takeBindablePort' ports
                put env { ssAllocatedPorts = M.insert name port $ ssAllocatedPorts env
                        , ssAvailablePorts = ports' }
                return port
  where takeBindablePort' pl = do
          pl' <- dropWhileM (liftM not . isBindable) pl
          return (head pl', tail pl')

isBindable :: PortNumber -> IO Bool
isBindable p = withSocketsDo $ do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  let sa = SockAddrInet p localhost
  r <- (bind s sa >> isBound s)
         `catch` ((\_ -> return False) :: SomeException -> IO Bool)
  close s
  return $! r

startProcess :: SandboxedProcess -> Sandbox SandboxedProcess
startProcess sp =
  case spInstance sp of
    Nothing -> startProcess'
    Just (RunningInstance {}) -> return sp
    Just (StoppedInstance {}) -> startProcess'
  where
    startProcess' :: Sandbox SandboxedProcess
    startProcess' = do
      bin <- getProcessBinary sp
      args <- mapM (liftIO . expand) $ spArgs sp
      (hOutRO, hOutRW, hErrRW) <- case spCapture sp of
                                    Just co -> liftIO $ do (pRO, pRW) <- createPipe
                                                           hRO <- fdToHandle pRO
                                                           hRW <- fdToHandle pRW
                                                           case co of
                                                             CaptureStdout -> return (Just hRO, UseHandle hRW, Inherit)
                                                             CaptureStderr -> return (Just hRO, Inherit, UseHandle hRW)
                                                             CaptureBoth -> return (Just hRO, UseHandle hRW, UseHandle hRW)
                                    Nothing -> return (Nothing, Inherit, Inherit)
      (Just ih, _, _, ph) <- liftIO $ createProcess $ (proc bin args) { std_in = CreatePipe
                                                                      , std_out = hOutRW
                                                                      , std_err = hErrRW }
      when (isJust $ spWait sp) $ liftIO . threadDelay $ fromJust (spWait sp) * secondInµs
      errno <- liftIO $ getProcessExitCode ph
      case errno of
        Nothing -> updateProcess sp { spInstance = Just $ RunningInstance ph ih hOutRO }
        Just errno' -> throwError $ "Process " ++ spName sp ++ " not running.\n\
                                    \ - command-line: " ++ formatCommandLine bin args ++ "\n\
                                    \ - exit code: " ++ show errno' 

formatCommandLine :: String -> [String] -> String
formatCommandLine bin args = unwords $ bin : args

stopProcess :: SandboxedProcess -> Sandbox SandboxedProcess
stopProcess sp =
  case spInstance sp of
    Just (RunningInstance ph _ _) -> do
      let wait = if isNothing $ spWait sp then 50000 else fromJust (spWait sp) * secondInµs `div` 5
      liftIO $ do terminateProcess ph
                  threadDelay wait
      stillRunning <- liftM isNothing $ liftIO $ getProcessExitCode ph
      when stillRunning $ liftIO $ killProcess ph
      stopProcess =<< getProcess (spName sp)
    _ -> return sp

hSignalProcess :: Signal -> ProcessHandle -> IO ()
hSignalProcess s h = do
  pid <- hGetProcessID h
  signalProcess s pid

killProcess :: ProcessHandle -> IO ()
killProcess = hSignalProcess sigKILL

hGetProcessID :: ProcessHandle -> IO ProcessID
hGetProcessID h = withProcessHandle h $ \x ->
  case x of
    OpenHandle pid -> return pid
    _ -> throwIO $ userError "Unable to retrieve child process ID."

interactWithProcess :: SandboxedProcess -> String -> Int -> Sandbox String
interactWithProcess sp input timeout = do
  hIn <- getProcessInputHandle sp
  hOut <- getProcessCapturedOutputHandle sp
  liftIO $ do B.hPutStr hIn $ B.pack input
              hFlush hIn
  b <- hReadWithTimeout hOut timeout
  return $! B.unpack b

getProcessInputHandle :: SandboxedProcess -> Sandbox Handle
getProcessInputHandle sp =
    case spInstance sp of
      Just (RunningInstance _ ih _) -> return ih
      _ -> throwError $ "No such handle for " ++ spName sp ++ ". \
                        \Is the process started?"

getProcessCapturedOutputHandle :: SandboxedProcess -> Sandbox Handle
getProcessCapturedOutputHandle sp =
  case spInstance sp of
    Just (RunningInstance _ _ (Just oh)) -> return oh
    _ -> throwError $ "No captured output handle for " ++ spName sp ++ ". \
                      \Is capture activated?"

getProcessBinary :: SandboxedProcess -> Sandbox FilePath
getProcessBinary sp = do
  existing <- liftIO $ findExecutables bins
  case existing of
    exBin:_ -> return exBin
    [] -> throwError $ "Unable to find the executable for the test process \""
                       ++ spName sp ++ "\"\r\n\
                       \Considered paths were: " ++ show bins
  where bins = getProcessCandidateBinaries sp

findExecutables :: [FilePath] -> IO [FilePath]
findExecutables paths =
   liftM catMaybes $ mapM tryBinary paths

tryBinary :: FilePath -> IO (Maybe FilePath)
tryBinary path = do
  expandedPath <- expand path
  canonicalizedPath <- tryIOError $ canonicalizePath expandedPath
  case canonicalizedPath of
    Left _ -> findExecutable expandedPath
    Right realPath -> findExecutable realPath

getProcessCandidateBinaries :: SandboxedProcess -> [FilePath]
getProcessCandidateBinaries sp =
  [ userBinary, binary, cwdBinary, pathBinary ]
  where binary = spBinary sp
        pathBinary = takeFileName binary
        cwdBinary = "." </> pathBinary
        userBinary = "${" ++ map toUpper (spName sp ++ "_bin") ++ "}"

expand :: String -> IO String
expand s =
  if '$' `elem` s then expandShell s
    else return s
  where
    expandShell :: String -> IO String
    expandShell p = do
      (_, Just outH, _, _) <- createProcess $
                                (shell $ "echo " ++ p) { std_out = CreatePipe }
      liftM (takeWhile (`notElem` "\r\n")) $ hGetContents outH

whenM :: Monad m => m Bool -> m () -> m ()
whenM = (. flip when) . (>>=)

-- | Sets a custom variable in the sandbox monad.
setVariable :: Serialize a
            => String    -- ^ Variable key for future reference
            -> a         -- ^ Variable value
            -> Sandbox a
setVariable name new = do
  env <- get
  put $ env { ssVariables = M.insert name (encode new) (ssVariables env) }
  return new

-- | Checks that a custom sandbox variable is set.
checkVariable :: String       -- ^ Variable key
              -> Sandbox Bool
checkVariable name = do
  env <- get
  return $ M.member name $ ssVariables env

-- | Returns the value of a previously set sandbox variable (or a provided default value if unset)
getVariable :: Serialize a
            => String    -- ^ Variable key
            -> a         -- ^ Default value if not found
            -> Sandbox a
getVariable name defval = do
  env <- get
  let var = case M.lookup name $ ssVariables env of
              Nothing -> Right defval
              Just var' -> decode var'
  either throwError return var

-- | Unsets a custom variable.
unsetVariable :: String     -- ^ Variable key
              -> Sandbox ()
unsetVariable name = do
  env <- get
  void $ put env { ssVariables = M.delete name $ ssVariables env }

isVerbose :: Sandbox Bool
isVerbose = getVariable verbosityKey True

verbosityKey :: String
verbosityKey = "__VERBOSITY__"

displayBanner :: Sandbox ()
displayBanner = do
  displayed <- checkVariable var
  unless displayed $ get >>= liftIO . putStrLn . pretty
  void $ setVariable var True
  where var = "__BANNER__DISPLAYED__"

installSignalHandlers :: Sandbox ()
installSignalHandlers = do
  installed <- checkVariable var
  unless installed $ liftIO . void $ do installHandler sigTERM handler Nothing
                                        installHandler sigQUIT handler Nothing
                                        installHandler sigABRT handler Nothing
  void $ setVariable var True
  where var = "__HANDLERS_INSTALLED__"
        handler = Catch $ signalProcess sigINT =<< getProcessID

-- Structures to store Sandbox options for future use.
-- Not expected to be used directly by the user.

data SandboxSeed = SandboxFixedSeed Int
                 | SandboxRandomSeed
  deriving (Generic)

instance Serialize (SandboxSeed)

data SandboxTestOptions = SandboxTestOptions {
    stoSeed :: Maybe SandboxSeed
  , stoMaximumGeneratedTests :: Maybe Int
  , stoMaximumUnsuitableGeneratedTests :: Maybe Int
  , stoMaximumTestSize :: Maybe Int
  } deriving (Generic)

instance Serialize (SandboxTestOptions)

putOptions :: SandboxTestOptions -> Sandbox ()
putOptions = void . setVariable optionsVariable . Just

getOptions :: Sandbox (Maybe SandboxTestOptions)
getOptions = getVariable optionsVariable Nothing

optionsVariable :: String
optionsVariable = "__TEST_OPTIONS__"
