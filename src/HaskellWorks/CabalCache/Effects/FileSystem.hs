{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.FileSystem
  ( FileSystem(..)
  , runEffFileSystem
  , readFile
  , writeFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , createSystemTempDirectory
  , removeDirectoryRecursive

  , withSystemTempDirectory
  ) where

import Polysemy
import Polysemy.Resource (Resource, bracket)
import Prelude           hiding (readFile, writeFile)

import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory     as IO
import qualified System.IO.Temp       as IO

data FileSystem m a where
  ReadFile                  :: FilePath -> FileSystem m LBS.ByteString
  WriteFile                 :: FilePath -> LBS.ByteString -> FileSystem m ()
  CreateDirectoryIfMissing  :: FilePath -> FileSystem m ()
  DoesDirectoryExist        :: FilePath -> FileSystem m Bool
  CreateSystemTempDirectory :: FilePath -> FileSystem m FilePath
  RemoveDirectoryRecursive  :: FilePath -> FileSystem m ()

makeSem ''FileSystem

runEffFileSystem :: Member (Embed IO) r
  => Sem (FileSystem ': r) a
  -> Sem r a
runEffFileSystem = interpret $ \case
  ReadFile fp -> embed $ LBS.readFile fp
  WriteFile fp contents -> embed $ LBS.writeFile fp contents
  CreateDirectoryIfMissing fp -> embed $ IO.createDirectoryIfMissing True fp
  DoesDirectoryExist fp -> embed $ IO.doesDirectoryExist fp
  CreateSystemTempDirectory fp -> do
    pp <- embed IO.getCanonicalTemporaryDirectory
    embed $ IO.createTempDirectory pp fp
  RemoveDirectoryRecursive fp -> embed $ IO.removeDirectoryRecursive fp

withSystemTempDirectory :: Members '[FileSystem, Resource] r
  => FilePath
  -> (FilePath -> Sem r a)
  -> Sem r a
withSystemTempDirectory fp = bracket (createSystemTempDirectory fp) removeDirectoryRecursive
