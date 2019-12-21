{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.GhcPkg
  ( testAvailability
  , recache
  , init
  ) where

import Control.Lens
import Data.Generics.Product.Any (the)
import Polysemy
import Polysemy.Fail
import Prelude                   hiding (init)

import qualified HaskellWorks.CabalCache.Effects.Process as E
import qualified HaskellWorks.CabalCache.Types           as Z
import qualified System.Exit                             as IO

runGhcPkg :: Members
    [ E.Process
    , Fail
    ] r
  => Z.CompilerContext
  -> [String]
  -> Sem r (Either IO.ExitCode ())
runGhcPkg cc args = do
  hGhcPkg   <- E.system ((cc ^. the @"ghcPkgCmd") <> args)
  exitCode  <- E.waitForProcess hGhcPkg
  case exitCode of
    IO.ExitFailure _ -> return (Left exitCode)
    _                -> return (Right ())

testAvailability :: Members
  [ E.Process
  , Fail
  ] r
  => Z.CompilerContext
  -> Sem r ()
testAvailability cc = do
  result <- runGhcPkg cc ["--version"]
  case result of
    Left _   -> fail "ghcpkg command line tool unavailable"
    Right () -> return ()

recache :: Members
  [ E.Process
  , Fail
  ] r
  => Z.CompilerContext
  -> FilePath
  -> Sem r ()
recache cc packageDb = do
  result <- runGhcPkg cc ["recache", "--package-db", packageDb]
  case result of
    Left _   -> fail "pkgdb recache failed"
    Right () -> return ()

init :: Members
  [ E.Process
  , Fail
  ] r
  => Z.CompilerContext
  -> FilePath
  -> Sem r ()
init cc packageDb = do
  result <- runGhcPkg cc ["init", packageDb]
  case result of
    Left _   -> fail "pkgdb inititialisation failed"
    Right () -> return ()
