{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Process
  ( Process(..)
  , runEffProcess
  , spawnProcess
  , system
  , waitForProcess
  ) where

import Polysemy
import Polysemy.Fail

import qualified System.Exit    as IO
import qualified System.Process as IO

data Process m a where
  SpawnProcess   :: String -> [String] -> Process m IO.ProcessHandle
  WaitForProcess :: IO.ProcessHandle -> Process m IO.ExitCode

makeSem ''Process

runEffProcess :: Member (Embed IO) r
  => Sem (Process ': r) a
  -> Sem r a
runEffProcess = interpret $ \case
  SpawnProcess cmd args -> embed $ IO.spawnProcess cmd args
  WaitForProcess hProcess -> embed $ IO.waitForProcess hProcess

system
  :: Members
      [ Process
      , Fail
      ] r
  => [String]
  -> Sem r IO.ProcessHandle
system (cmd:args) = spawnProcess cmd args
system []         = error "No command supplied" -- TODO Better error handling
