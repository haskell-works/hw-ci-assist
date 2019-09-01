{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Control.Resource
  ( ReleaseToken
  , emptyReleaseToken
  , unpackResourceT
  , release
  ) where

import Control.Monad.IO.Class                (MonadIO, liftIO)
import Control.Monad.Trans.Resource          (ResourceT, createInternalState, getInternalState, runInternalState, unprotect)
import Control.Monad.Trans.Resource.Internal (ReleaseKey (..), ReleaseMap (..))
import Data.Acquire                          (ReleaseType (..))
import Data.IORef                            (IORef, readIORef)
import Data.Maybe                            (catMaybes)

import qualified Data.IntMap as IntMap

newtype ReleaseToken = ReleaseToken (IO ())

emptyReleaseToken :: ReleaseToken
emptyReleaseToken = ReleaseToken (pure ())

release :: ReleaseToken -> IO ()
release (ReleaseToken action) = action

unpackResourceT :: ResourceT IO a -> IO (ReleaseToken, a)
unpackResourceT f = do
  ist <- createInternalState
  flip runInternalState ist $ do
    res <- f
    actions <- getAllKeys >>= traverse unprotect
    let cleanup = sequence_ (catMaybes actions)
    pure (ReleaseToken cleanup, res)

--------------------------------------------------------------------------------------
getAllKeys :: MonadIO m => ResourceT m [ReleaseKey]
getAllKeys = do
  iom <- getInternalState
  m   <- getMap iom
  pure $ ReleaseKey iom <$> IntMap.keys m

getMap :: MonadIO m => IORef ReleaseMap -> m (IntMap.IntMap (ReleaseType -> IO ()))
getMap iom = liftIO $
  readIORef iom >>= \case
    ReleaseMap _ _ m -> pure m
    ReleaseMapClosed -> pure IntMap.empty
