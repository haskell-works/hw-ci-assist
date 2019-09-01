{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.GhcPkg
  ( testAvailability
  , recache
  , init
  ) where

import Polysemy
import Polysemy.Fail
import Prelude       hiding (init)

import qualified HaskellWorks.CabalCache.Effects.Process as E
import qualified System.Exit                             as IO

runGhcPkg :: Members
    [ E.Process
    , Fail
    ] r
  => [String]
  -> Sem r (Either IO.ExitCode ())
runGhcPkg args = do
  hGhcPkg <- E.spawnProcess "ghc-pkg" args
  exitCode <- E.waitForProcess hGhcPkg
  case exitCode of
    IO.ExitFailure _ -> return (Left exitCode)
    _                -> return (Right ())

testAvailability :: Members
  [ E.Process
  , Fail
  ] r
  => Sem r ()
testAvailability = do
  result <- runGhcPkg ["--version"]
  case result of
    Left _   -> fail "ghcpkg command line tool unavailable"
    Right () -> return ()

recache :: Members
  [ E.Process
  , Fail
  ] r
  => FilePath
  -> Sem r ()
recache packageDb = do
  result <- runGhcPkg ["recache", "--package-db", packageDb]
  case result of
    Left _   -> fail "pkgdb recache failed"
    Right () -> return ()

init :: Members
  [ E.Process
  , Fail
  ] r
  => FilePath
  -> Sem r ()
init packageDb = do
  result <- runGhcPkg ["init", packageDb]
  case result of
    Left _   -> fail "pkgdb inititialisation failed"
    Right () -> return ()
