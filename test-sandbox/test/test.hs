{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Test.Hspec hiding (shouldReturn,shouldBe)
import Test.Hspec.Expectations.Lifted
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert,monadicIO)
import Test.Sandbox
import qualified Test.Sandbox.Internals as I
import Text.Heredoc
import qualified Text.Mustache as H
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Map as M
import System.Posix.Files
import System.Exit (ExitCode(..))
import Data.IORef
import Data.Char
import Control.Concurrent
import Data.String.Conversions

import Network.Socket
import Network.Socket.ByteString (sendAll,recv)
import Network.Run.TCP
import System.IO
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Pair)


a2b :: String -> String
a2b [] = []
a2b ('a':xs) = 'b':xs
a2b (x:xs) = x:a2b xs

a2btest :: I.SandboxStateRef -> [Char] -> Property
a2btest ref str' = a2btest' ref $ filter (\c -> isAlphaNum c && isAscii c) str'

a2btest' :: I.SandboxStateRef -> [Char] -> Property
a2btest' ref str' =
  monadicIO $ do
    liftIO $ print str'
    v <- liftIO $ runSandbox' ref $ interactWith "sed_regex" (str' ++ "\n") 1
    assert $ v == ((a2b str') ++ "\n")

mainEcho :: IO ()
mainEcho = do
  let val = "hoge\n"
  runTCPServer Nothing "12181" (talk val)
  where
    talk val s = do
      sendAll s val
      msg <- recv s 1024
      sendAll s msg

main :: IO ()
main = withSandbox $ \gref -> do
  hspec $ do
    describe "Basic Test" $ do
      it "Run server like nc : port 12181" $ do
        runSandbox' gref $ do
          file <- setFile "ncfile1"
                  [str|{-# LANGUAGE OverloadedStrings #-}
                      |import qualified Data.ByteString as B
                      |import Network.Socket.ByteString
                      |import Network.Socket
                      |import Network.Run.TCP
                      |main = do
                      |  let val = "hoge\n"
                      |  runTCPServer Nothing "12181" (talk val)
                      |  where
                      |    talk val s = do
                      |      sendAll s val
                      |      msg <- recv s 1024
                      |      B.putStr msg
                      |]
          --liftIO $ setExecuteMode file
          start =<< register "ncserver1" "runghc" [file] def { psCapture = Just CaptureStdout }
      it "isBinable' 12181" $ do
        I.isBindable' 12181 `shouldReturn` False
      it "isBinable' 12180" $ do
        I.isBindable' 12180 `shouldReturn` True
      it "Test isBinable 12181" $ do
        I.isBindable 12181 `shouldReturn` False
      it "Test isBinable 12180" $ do
        I.isBindable 12180 `shouldReturn` True
      it "Test sendTo" $ do
        runSandbox' gref $ do
          p <- getPort "ncport"
          file <- setFile' "ncfile"
                  [("port",toJSON p)]
                  [str|{-# LANGUAGE OverloadedStrings #-}
                      |import qualified Data.ByteString as B
                      |import Network.Socket.ByteString
                      |import Network.Socket
                      |import Network.Run.TCP
                      |main = do
                      |  let val = "hoge\n"
                      |  runTCPServer Nothing "{{port}}" (talk val)
                      |  where
                      |    talk val s = do
                      |      sendAll s val
                      |      msg <- recv s 1024
                      |      B.putStr msg
                      |]
          liftIO $ I.isBindable p `shouldReturn` True
          -- liftIO $ setExecuteMode file
          start =<< register "ncserver" "runghc" [file] def { psCapture = Just CaptureStdout }
          liftIO $ I.isBindable p `shouldReturn` False
          sendTo "ncport" "hogehoge\n" 1 `shouldReturn` "hoge\n"
      it "interactive Test by sandbox" $ do
        sandbox "hogehoge" $ do
          start =<< register "sed_regex" "sed" [ "-u", "s/a/b/" ] def { psCapture = Just CaptureStdout }
          v <- interactWith "sed_regex" "a\n" 1
          liftIO $ v `shouldBe` "b\n"
      it "interactive Test by withSandbox" $ do
        withSandbox $ \ref -> do
          val <- runSandbox' ref $ do
            start =<< register "sed_regex" "sed" [ "-u", "s/a/b/" ] def { psCapture = Just CaptureStdout }
            interactWith "sed_regex" "a\n" 1
          val `shouldBe` "b\n"
      it "interactive Test : QuickCheck(setup)" $ do
        val <- runSandbox' gref $ do
          start =<< register "sed_regex" "sed" [ "-u", "s/a/b/" ] def { psCapture = Just CaptureStdout }
          interactWith "sed_regex" "a\n" 1
        val `shouldBe` "b\n"
      it "interactive Test : QuickCheck(run)" $
        property $ a2btest gref
      it "setFile" $ do
        withSandbox $ \ref -> do
          let content =
                  [str|#!/bin/env bash
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          putStr content
          file <- runSandbox' ref $ setFile "str1" content
          readFile file `shouldReturn`  content
      it "set/getVariable" $ do
        withSandbox $ \ref -> do
          runSandbox' ref $ do
            v <- I.isVerbose
            liftIO $ v `shouldBe` True
            _ <- setVariable I.verbosityKey False
            v' <- I.isVerbose
            liftIO $ v' `shouldBe` False
          runSandbox' ref I.isVerbose `shouldReturn` False
          _ <- runSandbox' ref $ setVariable I.verbosityKey True
          runSandbox' ref I.isVerbose `shouldReturn` True
      it ": run" $ do
        withSandbox $ \ref -> runSandbox' ref $ do
          file <- setFile "echoecho"
                  [str|#!/usr/bin/env bash
                      |echo "echoecho"
                      |]
          liftIO $ setExecuteMode file

          pname1 <- register "echo1" file [] def
          (exitCode1, mStr1) <- run pname1 1
          liftIO $ exitCode1 `shouldBe` ExitSuccess
          liftIO $ mStr1 `shouldBe` Nothing

          pname2 <- register "echo2" file [] def { psCapture = Just CaptureStdout }
          (exitCode2, mStr2) <- run pname2 1
          liftIO $ exitCode2 `shouldBe` ExitSuccess
          liftIO $ mStr2 `shouldBe` Just "echoecho\n"

#if defined(__MACOSX__) ||  defined(__WIN32__)
#else
    describe "signal test" $ do
      it "send kill signal for process groups" $ do
        val <- newIORef $ error "do not eval this"
        withSandbox $ \ref -> runSandbox' ref $ do
          file <- setFile "str1"
                  [str|#!/usr/bin/env bash
                      |trap "echo catch signal" 1 2 3 15
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          liftIO $ setExecuteMode file
          fil2 <- setFile' "str2"
                  [("file",toJSON file)]
                  [str|#!/usr/bin/env bash
                      |trap "echo catch signal" 1 2 3 15
                      |{{file}}&
                      |{{file}}&
                      |{{file}}&
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          liftIO $ setExecuteMode fil2
          _ <- register "scr1" fil2 [] def
          startAll
          liftIO $ writeIORef val ref
          pids <- I.getAvailablePids
          liftIO $ threadDelay $ 1 * 1000 * 1000
          liftIO $ (length pids >= 4) `shouldBe` True
        ref' <- readIORef val
        let chkpid  :: IO Int
            chkpid = do
              threadDelay (5 * 1000 * 1000)
              pids <- runSandbox' ref' $ I.getAvailablePids
              return $ length pids
        chkpid `shouldReturn` 0

      it "send kill signal for process groups" $ do
        sandbox "test" $ do
          file <- setFile "str1"
                  [str|#!/usr/bin/env bash
                      |trap "echo catch signal" 1 2 3 15
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          liftIO $ setExecuteMode file
          fil2 <- setFile' "str2"
                  [("file",toJSON file)]
                  [str|#!/usr/bin/env bash
                      |trap "echo catch signal" 1 2 3 15
                      |{{file}}&
                      |{{file}}&
                      |{{file}}&
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          liftIO $ setExecuteMode fil2
          _ <- register "scr1" fil2 [] def
          startAll
          pids <- I.getAvailablePids
          liftIO $ (length pids >= 4) `shouldBe` True
      it "not send kill signal for process groups" $ do
        val <- newIORef $ error "do not eval this"
        withSandbox $ \ref -> runSandbox' ref $ do
          file <- setFile "str1"
                  [str|#!/usr/bin/env bash
                      |trap "echo catch signal" 1 2 3 15
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          liftIO $ setExecuteMode file
          fil2 <- setFile' "str2"
                  [("file",toJSON file)]
                  [str|#!/usr/bin/env bash
                      |trap "echo catch signal" 1 2 3 15
                      |{{file}}&
                      |{{file}}&
                      |{{file}}&
                      |while true ; do echo hhh ; sleep 1;done
                      |]
          liftIO $ setExecuteMode fil2
          _ <- register "scr1" fil2 [] def
          startAll
          _ <- setVariable I.cleanUpKey False
          pids <- I.getAvailablePids
          liftIO $ writeIORef val ref
          liftIO $ do
            (length pids >= 4) `shouldBe` True
        ref' <- readIORef val
        pids <- runSandbox' ref' $ I.getAvailablePids
        (length pids > 0 ) `shouldBe` True
        pids' <- runSandbox' ref' $ do
          I.cleanUpProcesses
          I.getAvailablePids
        length pids' `shouldBe` 0
#endif

setFile' :: String
         -> [Pair]
         -> String
         -> Sandbox FilePath
setFile' filename keyValues template  = do
  str' <- case (H.compileMustacheText (H.PName (cs filename)) (cs template)) of 
    Left err -> fail $ show err
    Right template -> do
      return $ H.renderMustache template (object keyValues)
  setFile filename $ cs str'

setExecuteMode :: FilePath -> IO ()
setExecuteMode file = do
  stat <- getFileStatus file
  setFileMode file (fileMode stat `unionFileModes` ownerExecuteMode)
