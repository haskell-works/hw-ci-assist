{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module HaskellWorks.CabalCache.Effects.Tar
  ( extractTar
  ) where

import HaskellWorks.CabalCache.Error
import HaskellWorks.CabalCache.Show
import Polysemy
import Polysemy.Error
import Prelude                       hiding (init)

import qualified HaskellWorks.CabalCache.Effects.Process as E
import qualified System.Exit                             as IO

newtype TarError = TarExtractError Int
  deriving (Eq, Show)

instance ErrorMessage TarError where
  errorMessage (TarExtractError exitFailureCode) = "Failed to extract tar.  Exit code: " <> tshow exitFailureCode

extractTar ::
    Members
      [ E.Process
      , Error TarError
      ] r
  => FilePath
  -> FilePath
  -> Sem r ()
extractTar tarFile targetPath = do
  process <- E.spawnProcess "tar" ["-C", targetPath, "-zxf", tarFile]
  exitCode <- E.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throw (TarExtractError n)
