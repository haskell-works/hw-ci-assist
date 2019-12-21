{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module HaskellWorks.CabalCache.Core
  ( PackageInfo(..)
  , Tagged(..)
  , Presence(..)
  , getPackages
  , relativePaths
  , loadPlan
  , mkCompilerContext
  ) where

import Control.DeepSeq                  (NFData)
import Control.Lens                     hiding ((<.>))
import Control.Monad                    (forM)
import Control.Monad.Except
import Data.Aeson                       (eitherDecode)
import Data.Bifunctor                   (first)
import Data.Bool                        (bool)
import Data.Generics.Product.Any        (the)
import Data.Semigroup                   ((<>))
import Data.String
import Data.Text                        (Text)
import GHC.Generics                     (Generic)
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.Error
import System.FilePath                  ((<.>), (</>))

import qualified Data.ByteString.Lazy           as LBS
import qualified Data.List                      as List
import qualified Data.Text                      as T
import qualified HaskellWorks.CabalCache.IO.Tar as IO
import qualified HaskellWorks.CabalCache.Types  as Z
import qualified System.Directory               as IO

{-# ANN module ("HLint: ignore Monoid law, left identity" :: String) #-}

type PackageDir = FilePath
type ConfPath   = FilePath
type Library    = FilePath

data Presence   = Present | Absent deriving (Eq, Show, NFData, Generic)

data Tagged a t = Tagged
  { value :: a
  , tag   :: t
  } deriving (Eq, Show, Generic, NFData)

data PackageInfo = PackageInfo
  { compilerId :: Z.CompilerId
  , packageId  :: Z.PackageId
  , packageDir :: PackageDir
  , confPath   :: Tagged ConfPath Presence
  , libs       :: [Library]
  } deriving (Show, Eq, Generic, NFData)

mkCompilerContext :: MonadIO m => Z.PlanJson -> ExceptT Text m Z.CompilerContext
mkCompilerContext plan = do
  compilerVersion <- T.stripPrefix "ghc-" (plan ^. the @"compilerId") & nothingToError "No compiler version available in plan"
  let ghcPkgCmd = "ghc-pkg-" <> compilerVersion
  ghcPkgCmdPath <- liftIO (IO.findExecutable (T.unpack ghcPkgCmd)) >>= nothingToError (ghcPkgCmd <> " is not in path")
  return (Z.CompilerContext [ghcPkgCmdPath])

relativePaths :: FilePath -> PackageInfo -> [IO.TarGroup]
relativePaths basePath pInfo =
  [ IO.TarGroup basePath $ mempty
      <> (pInfo ^. the @"libs")
      <> [packageDir pInfo]
  , IO.TarGroup basePath $ mempty
      <> ([pInfo ^. the @"confPath"] & filter ((== Present) . (^. the @"tag")) <&> (^. the @"value"))
  ]

getPackages :: FilePath -> Z.PlanJson -> IO [PackageInfo]
getPackages basePath planJson = forM packages (mkPackageInfo basePath compilerId')
  where compilerId' :: Text
        compilerId' = planJson ^. the @"compilerId"
        packages :: [Z.Package]
        packages = planJson ^. the @"installPlan"

loadPlan :: IO (Either AppError Z.PlanJson)
loadPlan = first fromString . eitherDecode <$> LBS.readFile ("dist-newstyle" </> "cache" </> "plan.json")

-------------------------------------------------------------------------------
mkPackageInfo :: FilePath -> Z.CompilerId -> Z.Package -> IO PackageInfo
mkPackageInfo basePath cid pkg = do
  let pid               = pkg ^. the @"id"
  let compilerPath      = basePath </> T.unpack cid
  let relativeConfPath  = T.unpack cid </> "package.db" </> T.unpack pid <.> ".conf"
  let absoluteConfPath  = basePath </> relativeConfPath
  let libPath           = compilerPath </> "lib"
  let relativeLibPath   = T.unpack cid </> "lib"
  let libPrefix         = "libHS" <> pid
  absoluteConfPathExists <- IO.doesFileExist absoluteConfPath
  libFiles <- getLibFiles relativeLibPath libPath libPrefix
  return PackageInfo
    { compilerId  = cid
    , packageId   = pid
    , packageDir  = T.unpack cid </> T.unpack pid
    , confPath    = Tagged relativeConfPath (bool Absent Present absoluteConfPathExists)
    , libs        = libFiles
    }

getLibFiles :: FilePath -> FilePath -> Text -> IO [Library]
getLibFiles relativeLibPath libPath libPrefix = do
  libExists <- IO.doesDirectoryExist libPath
  if libExists
     then fmap (relativeLibPath </>) . filter (List.isPrefixOf (T.unpack libPrefix)) <$> IO.listDirectory libPath
     else pure []
