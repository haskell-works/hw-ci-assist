{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive
  ) where

import Antiope.Core                         (runResAws, toText)
import Antiope.Env                          (LogLevel, mkEnv)
import App.Commands.Options.Parser          (optsSyncFromArchive)
import App.Static                           (homeDirectory)
import Codec.Archive.Tar                    (mapEntriesNoFail)
import Control.Lens                         hiding ((<.>))
import Control.Monad                        (unless, when)
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Trans.Resource         (runResourceT)
import Data.Generics.Product.Any            (the)
import Data.Semigroup                       ((<>))
import HaskellWorks.Ci.Assist.Core          (PackageInfo (..), getPackages, loadPlan)
import HaskellWorks.Ci.Assist.Location      ((<.>), (</>))
import HaskellWorks.Ci.Assist.PackageConfig (replacePrefix, unTemplateConfig)
import HaskellWorks.Ci.Assist.Tar           (mapEntriesWith, rewritePath)
import Network.AWS.Types                    (Region (Oregon))
import Options.Applicative                  hiding (columns)
import System.Directory                     (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types        as Z
import qualified Codec.Archive.Tar                 as F
import qualified Codec.Compression.GZip            as F
import qualified Data.Text                         as T
import qualified HaskellWorks.Ci.Assist.GhcPkg     as GhcPkg
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified HaskellWorks.Ci.Assist.IO.Lazy    as IO
import qualified HaskellWorks.Ci.Assist.Types      as Z
import qualified System.IO                         as IO
import qualified UnliftIO.Async                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  let archiveUri = opts ^. the @"archiveUri"
  CIO.putStrLn $ "Archive URI: " <> toText archiveUri

  GhcPkg.testAvailability

  mbPlan <- loadPlan
  case mbPlan of
    Right planJson -> do
      env <- mkEnv (opts ^. the @"region") (\_ _ -> pure ())
      let archivePath                 = archiveUri </> (planJson ^. the @"compilerId")
      let baseDir                     = opts ^. the @"storePath"
      let storeCompilerPath           = baseDir </> (planJson ^. the @"compilerId" . to T.unpack)
      let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
      let storeCompilerLibPath        = storeCompilerPath </> "lib"

      CIO.putStrLn "Creating store directories"
      createDirectoryIfMissing True baseDir
      createDirectoryIfMissing True storeCompilerPath
      createDirectoryIfMissing True storeCompilerLibPath

      storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

      unless storeCompilerPackageDbPathExists $ do
        CIO.putStrLn "Package DB missing. Creating Package DB"
        GhcPkg.init storeCompilerPackageDbPath

      packages <- getPackages baseDir planJson

      IO.pooledForConcurrentlyN_ (opts ^. the @"threads") packages $ \pInfo -> do
        let archiveFile = archiveUri </> T.pack (packageDir pInfo) <.> ".tar.gz"
        let packageStorePath = baseDir </> packageDir pInfo
        storeDirectoryExists <- doesDirectoryExist packageStorePath
        arhiveFileExists <- runResourceT $ IO.resourceExists env archiveFile
        when (not storeDirectoryExists && arhiveFileExists) $ do
          runResAws env $ do
            maybeArchiveFileContents <- IO.readResource env archiveFile
            case maybeArchiveFileContents of
              Just archiveFileContents -> do
                CIO.putStrLn $ "Extracting " <> toText archiveFile

                let shortPath = (planJson ^. the @"compilerId" . to T.unpack) </> "pkg"
                let entries = F.read (F.decompress archiveFileContents)
                let entries' = case confPath pInfo of
                                Nothing   -> entries
                                Just conf -> entries & mapEntriesWith (== conf) (unTemplateConfig baseDir)

                let entries'' = entries & mapEntriesNoFail (rewritePath (replacePrefix shortPath (packageDir pInfo)))

                liftIO $ F.unpack baseDir entries''
              Nothing -> do
                CIO.putStrLn $ "Archive unavilable: " <> toText archiveFile

      CIO.putStrLn "Recaching package database"
      GhcPkg.recache storeCompilerPackageDbPath

    Left errorMessage -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> T.pack errorMessage

  return ()

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = command "sync-from-archive"  $ flip info idm $ runSyncFromArchive <$> optsSyncFromArchive
