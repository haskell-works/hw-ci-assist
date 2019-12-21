{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.PolySyncFromArchive
  ( cmdPolySyncFromArchive
  ) where

import Antiope.Core                     (runResAws, toText)
import Antiope.Env                      (mkEnv)
import App.Commands.Options.Parser      (optsPolySyncFromArchive)
import Control.Lens                     hiding ((<.>))
import Control.Monad                    (unless, void, when)
import Control.Monad.Catch              (MonadCatch)
import Control.Monad.Except
import Control.Monad.IO.Class           (liftIO)
import Data.ByteString.Lazy.Search      (replace)
import Data.Generics.Product.Any        (the)
import Data.Maybe
import Data.Semigroup                   ((<>))
import Foreign.C.Error                  (eXDEV)
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.IO.Error (catchErrno, exceptWarn, maybeToExcept)
import HaskellWorks.CabalCache.Location ((<.>), (</>))
import HaskellWorks.CabalCache.Metadata (loadMetadata)
import HaskellWorks.CabalCache.Show
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              hiding (columns)
import System.Directory                 (createDirectoryIfMissing, doesDirectoryExist)
import Polysemy
import Network.AWS.Types                (LogLevel(..))

import qualified App.Commands.Options.Types                       as Z
import qualified Control.Concurrent.STM                           as STM
import qualified Data.ByteString.Char8                            as C8
import qualified Data.ByteString.Lazy                             as LBS
import qualified Data.Map                                         as M
import qualified Data.Map.Strict                                  as Map
import qualified Data.Text                                        as T
import qualified HaskellWorks.CabalCache.AWS.Env                  as AWS
import qualified HaskellWorks.CabalCache.Concurrent.DownloadQueue as DQ
import qualified HaskellWorks.CabalCache.Concurrent.Fork          as IO
import qualified HaskellWorks.CabalCache.Core                     as Z
import qualified HaskellWorks.CabalCache.Effects.Aws              as E
import qualified HaskellWorks.CabalCache.Effects.Console          as E
import qualified HaskellWorks.CabalCache.Effects.Exit             as E
import qualified HaskellWorks.CabalCache.Effects.FileSystem       as E
import qualified HaskellWorks.CabalCache.Effects.GhcPkg           as E
import qualified HaskellWorks.CabalCache.Effects.Log              as E
import qualified HaskellWorks.CabalCache.Effects.Plan             as E
import qualified HaskellWorks.CabalCache.Effects.Process          as E
import qualified HaskellWorks.CabalCache.GhcPkg                   as GhcPkg
import qualified HaskellWorks.CabalCache.Hash                     as H
import qualified HaskellWorks.CabalCache.IO.Console               as CIO
import qualified HaskellWorks.CabalCache.IO.Lazy                  as IO
import qualified HaskellWorks.CabalCache.IO.Tar                   as IO
import qualified HaskellWorks.CabalCache.Types                    as Z
import qualified Polysemy                                         as E
import qualified Polysemy.Error                                   as E
import qualified Polysemy.Fail                                    as E
import qualified Polysemy.Final                                   as E
import qualified System.Directory                                 as IO
import qualified System.IO                                        as IO
import qualified System.IO.Temp                                   as IO
import qualified System.IO.Unsafe                                 as IO

{-# ANN module ("HLint: ignore Monoid law, left identity"   :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"          :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"                :: String) #-}

skippable :: Z.Package -> Bool
skippable package = package ^. the @"packageType" == "pre-existing"

runPolySyncFromArchive' :: Z.PolySyncFromArchiveOptions -> IO ()
runPolySyncFromArchive' opts = E.runFinal . E.embedToFinal @IO . E.runEffConsole . E.runEffLogConsole . E.exitAndPrintOnFail $ do  -- E.runEffExit . E.exitAndPrintOnFail $
  result <- runInEffect opts
    & E.runError @E.PlanError
    & E.runEffProcess
    & E.runEffFileSystem

  return ()

runInEffect :: Members '[
      E.Console
    , E.Error E.PlanError
    , E.Fail
    , E.FileSystem
    , E.Embed IO
    , E.Final IO
    , E.Log
    , E.Process
    ] r
  => Z.PolySyncFromArchiveOptions -> Sem r ()
runInEffect opts = do
  let storePath           = opts ^. the @"storePath"
  let archiveUri          = opts ^. the @"archiveUri"
  let threads             = opts ^. the @"threads"
  let awsLogLevel         = opts ^. the @"awsLogLevel"
  let versionedArchiveUri = archiveUri </> archiveVersion
  let storePathHash       = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUri    = versionedArchiveUri </> T.pack storePathHash

  E.putStrLn $ "Store path: "       <> toText storePath
  E.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  E.putStrLn $ "Archive URI: "      <> toText archiveUri
  E.putStrLn $ "Archive version: "  <> archiveVersion
  E.putStrLn $ "Threads: "          <> tshow threads
  E.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

  planJson <- E.loadPlan & E.successOrFail @E.PlanError

  compilerContextResult <- runExceptT $ Z.mkCompilerContext planJson

  case compilerContextResult of
    Right compilerContext -> do
      E.testAvailability compilerContext

      E.runEffMkAwsEnv $ do
        env <- E.awsMkEnv (opts ^. the @"region") (E.awsLogger (Just Error))

        return ()

      return ()

runPolySyncFromArchive :: Z.PolySyncFromArchiveOptions -> IO ()
runPolySyncFromArchive opts = do
  let storePath           = opts ^. the @"storePath"
  let archiveUri          = opts ^. the @"archiveUri"
  let threads             = opts ^. the @"threads"
  let awsLogLevel         = opts ^. the @"awsLogLevel"
  let versionedArchiveUri = archiveUri </> archiveVersion
  let storePathHash       = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUri    = versionedArchiveUri </> T.pack storePathHash

  CIO.putStrLn $ "Store path: "       <> toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  CIO.putStrLn $ "Archive URI: "      <> toText archiveUri
  CIO.putStrLn $ "Archive version: "  <> archiveVersion
  CIO.putStrLn $ "Threads: "          <> tshow threads
  CIO.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

  envAws <- mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)

  mbPlan <- Z.loadPlan
  case mbPlan of
    Right planJson -> do
      compilerContextResult <- runExceptT $ Z.mkCompilerContext planJson

      case compilerContextResult of
        Right compilerContext -> do
          -- E.testAvailability compilerContext
          envAws <- IO.unsafeInterleaveIO $ mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
          let compilerId                  = planJson ^. the @"compilerId"
          let storeCompilerPath           = storePath </> T.unpack compilerId
          let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
          let storeCompilerLibPath        = storeCompilerPath </> "lib"

          CIO.putStrLn "Creating store directories"
          createDirectoryIfMissing True storePath
          createDirectoryIfMissing True storeCompilerPath
          createDirectoryIfMissing True storeCompilerLibPath

          storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

          unless storeCompilerPackageDbPathExists $ do
            CIO.putStrLn "Package DB missing. Creating Package DB"
            GhcPkg.init compilerContext storeCompilerPackageDbPath

          packages <- Z.getPackages storePath planJson

          let installPlan = planJson ^. the @"installPlan"
          let planPackages = M.fromList $ fmap (\p -> (p ^. the @"id", p)) installPlan

          let planDeps0 = installPlan >>= \p -> fmap (p ^. the @"id", ) $ mempty
                <> (p ^. the @"depends")
                <> (p ^. the @"exeDepends")
                <> (p ^.. the @"components" . each . the @"lib" . each . the @"depends"    . each)
                <> (p ^.. the @"components" . each . the @"lib" . each . the @"exeDepends" . each)
          let planDeps  = planDeps0 <> fmap (\p -> ("[universe]", p ^. the @"id")) installPlan

          downloadQueue <- STM.atomically $ DQ.createDownloadQueue planDeps

          let pInfos = M.fromList $ fmap (\p -> (p ^. the @"packageId", p)) packages

          IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
            IO.createDirectoryIfMissing True (tempPath </> T.unpack compilerId </> "package.db")

            IO.forkThreadsWait threads $ DQ.runQueue downloadQueue $ \packageId -> case M.lookup packageId pInfos of
              Just pInfo -> do
                let archiveBaseName   = Z.packageDir pInfo <.> ".tar.gz"
                let archiveFile       = versionedArchiveUri </> T.pack archiveBaseName
                let scopedArchiveFile = scopedArchiveUri </> T.pack archiveBaseName
                let packageStorePath  = storePath </> Z.packageDir pInfo
                storeDirectoryExists <- doesDirectoryExist packageStorePath
                let maybePackage = M.lookup packageId planPackages

                case maybePackage of
                  Nothing -> do
                    CIO.hPutStrLn IO.stderr $ "Warning: package not found" <> packageId
                    return True
                  Just package -> if skippable package
                    then do
                      CIO.putStrLn $ "Skipping: " <> packageId
                      return True
                    else if storeDirectoryExists
                      then return True
                      else runResAws envAws $ onError (cleanupStorePath packageStorePath packageId) False $ do
                        (existingArchiveFileContents, existingArchiveFile) <- ExceptT $ IO.readFirstAvailableResource envAws [archiveFile, scopedArchiveFile]
                        CIO.putStrLn $ "Extracting: " <> toText existingArchiveFile

                        let tempArchiveFile = tempPath </> archiveBaseName
                        liftIO $ LBS.writeFile tempArchiveFile existingArchiveFileContents
                        IO.extractTar tempArchiveFile storePath

                        meta <- loadMetadata packageStorePath
                        oldStorePath <- maybeToExcept "store-path is missing from Metadata" (Map.lookup "store-path" meta)

                        case Z.confPath pInfo of
                          Z.Tagged conf _ -> do
                            let theConfPath = storePath </> conf
                            let tempConfPath = tempPath </> conf
                            confPathExists <- liftIO $ IO.doesFileExist theConfPath
                            when confPathExists $ do
                              confContents <- liftIO $ LBS.readFile theConfPath
                              liftIO $ LBS.writeFile tempConfPath (replace (LBS.toStrict oldStorePath) (C8.pack storePath) confContents)
                              liftIO $ catchErrno [eXDEV] (IO.renameFile tempConfPath theConfPath) (IO.copyFile tempConfPath theConfPath >> IO.removeFile tempConfPath)

                            return True
              Nothing -> do
                CIO.hPutStrLn IO.stderr $ "Warning: Invalid package id: " <> packageId
                return True

          CIO.putStrLn "Recaching package database"
          GhcPkg.recache compilerContext storeCompilerPackageDbPath

          failures <- STM.atomically $ STM.readTVar $ downloadQueue ^. the @"tFailures"

          forM_ failures $ \packageId -> CIO.hPutStrLn IO.stderr $ "Failed to download: " <> packageId

    Left appError -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> displayAppError appError

  return ()

cleanupStorePath :: (MonadIO m, MonadCatch m) => FilePath -> Z.PackageId -> AppError -> m ()
cleanupStorePath packageStorePath packageId e = do
  CIO.hPutStrLn IO.stderr $ "Warning: Sync failure: " <> packageId <> ", reason: " <> displayAppError e
  pathExists <- liftIO $ IO.doesPathExist packageStorePath
  when pathExists $ void $ IO.removePathRecursive packageStorePath

onError :: MonadIO m => (AppError -> m ()) -> a -> ExceptT AppError m a -> m a
onError h failureValue f = do
  result <- runExceptT $ catchError (exceptWarn f) handler
  case result of
    Left _  -> return failureValue
    Right a -> return a
  where handler e = lift (h e) >> return failureValue

cmdPolySyncFromArchive :: Mod CommandFields (IO ())
cmdPolySyncFromArchive = command "poly-sync-from-archive"  $ flip info idm $ runPolySyncFromArchive <$> optsPolySyncFromArchive
