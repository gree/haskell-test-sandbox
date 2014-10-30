{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Test.Sandbox.Process where
import System.Posix.Types
import System.Posix.Signals
import Text.Regex.Posix
import Data.Maybe
import Control.Exception
import Control.Monad
import System.Directory
import qualified Data.Set as S

#if defined(__MACOSX__) ||  defined(__WIN32__)
#else
data ProcessInfo = ProcessInfo {
   piPid  :: ProcessID
,  piStat :: String
,  piPpid :: ProcessID
,  piPgid :: ProcessGroupID
} deriving (Show,Eq,Read)

getProcessInfo :: String -> Maybe ProcessInfo
getProcessInfo v =
  if v =~ pattern
    then
      case v =~ pattern of
        [[_str,pid,stat,ppid,pgid]] -> Just $ ProcessInfo (read pid) stat (read ppid) (read pgid)
        _ -> Nothing
    else
      Nothing
  where
    pattern = "^([0-9]+) \\([^\\)]*\\) ([RSDZTW]) ([0-9]+) ([0-9]+) [0-9]+ .*"

getProcessInfos :: IO [ProcessInfo]
getProcessInfos = do
  dirs <- getDirectoryContents "/proc"
  let processes = filter ( =~ "[0-9]+") dirs
  stats <- forM processes $ \ps -> do {
    file <- (readFile $ "/proc/" ++ ps ++ "/stat") ;
    file `seq` return $ getProcessInfo file
    } `catch` (\(_ :: SomeException) -> return Nothing)
  return $ catMaybes stats

getProcessGroupIDs :: IO [ProcessGroupID]
getProcessGroupIDs = do
  infos <- getProcessInfos
  return $ map (\info -> piPgid info) infos

getProcessIDs :: [ProcessGroupID] -> IO [ProcessID]
getProcessIDs pgids = do
  infos <- getProcessInfos
  let pgids' = S.fromList $ pgids
  return $ map (\info -> piPid info) $ filter (\info -> S.member (piPgid info) pgids') infos
#endif

cleanUpProcessGroupIDs :: [ProcessGroupID] -> IO ()
cleanUpProcessGroupIDs pgids = do
  forM_ pgids $ \pgid -> do
    signalProcessGroup sigKILL pgid `catch`  (\(_::SomeException) -> return ())

