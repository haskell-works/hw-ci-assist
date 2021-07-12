{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HaskellWorks.CabalCache.Concurrent.DownloadQueue
  ( createDownloadQueue
  , runQueue
  ) where

import Control.Monad.IO.Class
import Data.Set               ((\\))

import qualified Control.Concurrent.STM                  as STM
import qualified Data.Relation                           as R
import qualified Data.Set                                as S
import qualified HaskellWorks.CabalCache.Concurrent.Type as Z

createDownloadQueue :: [(Z.ProviderId, Z.ConsumerId)] -> STM.STM Z.DownloadQueue
createDownloadQueue dependencies = do
  tDependencies <- STM.newTVar (R.fromList dependencies)
  tUploading    <- STM.newTVar S.empty
  tFailures     <- STM.newTVar S.empty
  return Z.DownloadQueue {..}

takeReady :: Z.DownloadQueue -> STM.STM (Maybe Z.PackageId)
takeReady Z.DownloadQueue {..} = do
  dependencies  <- STM.readTVar tDependencies
  uploading     <- STM.readTVar tUploading
  failures      <- STM.readTVar tFailures

  let ready = R.ran dependencies \\ R.dom dependencies \\ uploading \\ failures

  case S.lookupMin ready of
    Just packageId -> do
      STM.writeTVar tUploading (S.insert packageId uploading)
      return (Just packageId)
    Nothing -> if S.null (R.ran dependencies \\ R.dom dependencies \\ failures)
      then return Nothing
      else STM.retry

commit :: Z.DownloadQueue -> Z.PackageId -> STM.STM ()
commit Z.DownloadQueue {..} packageId = do
  dependencies  <- STM.readTVar tDependencies
  uploading     <- STM.readTVar tUploading

  STM.writeTVar tUploading    $ S.delete packageId uploading
  STM.writeTVar tDependencies $ R.withoutRan (S.singleton packageId) dependencies

failDownload :: Z.DownloadQueue -> Z.PackageId -> STM.STM ()
failDownload Z.DownloadQueue {..} packageId = do
  uploading <- STM.readTVar tUploading
  failures  <- STM.readTVar tFailures

  STM.writeTVar tUploading  $ S.delete packageId uploading
  STM.writeTVar tFailures   $ S.insert packageId failures

runQueue :: MonadIO m => Z.DownloadQueue -> (Z.PackageId -> m Bool) -> m ()
runQueue downloadQueue@Z.DownloadQueue {..} f = do
  maybePackageId <- liftIO $ STM.atomically $ takeReady downloadQueue

  case maybePackageId of
    Just packageId -> do
      success <- f packageId
      if success
        then liftIO $ STM.atomically $ commit downloadQueue packageId
        else liftIO $ STM.atomically $ failDownload downloadQueue packageId
      runQueue downloadQueue f

    Nothing -> do
      return ()
