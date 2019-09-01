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
  , waitForProcess
  ) where

import Polysemy

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
