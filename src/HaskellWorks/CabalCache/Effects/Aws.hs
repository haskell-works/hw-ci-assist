{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module HaskellWorks.CabalCache.Effects.Aws
  ( awsMkEnv
  , runEffAws
  , awsLogger
  ) where

import Antiope.Env                  (LogLevel (..), Region)
import Control.Concurrent           (myThreadId)
import Control.Monad
import HaskellWorks.CabalCache.Show
import Polysemy
import Polysemy.Final

import qualified Antiope.Env                         as IO
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Lazy.Char8          as LC8
import qualified Data.Text.Encoding                  as T
import qualified HaskellWorks.CabalCache.Effects.Log as E
import qualified Network.AWS.Types                   as AWS

data Aws m a where
  AwsMkEnv :: Region -> (LogLevel -> LBS.ByteString -> m ()) -> Aws m IO.Env

makeSem ''Aws

runEffAws
  :: Member (Final IO) r
  => Sem (Aws ': r) a
  -> Sem r a
runEffAws = interpretFinal $ \case
  AwsMkEnv region cb -> do
    cb' <- bindS (uncurry cb)
    s   <- getInitialStateS
    liftS $ IO.mkEnv region $ \loglvl str -> void $ cb' ((loglvl, str) <$ s)

awsLogger
  :: Members '[E.Log, Embed IO] r
  => Maybe AWS.LogLevel -> AWS.LogLevel -> LC8.ByteString
  -> Sem r ()
awsLogger maybeConfigLogLevel msgLogLevel message =
  forM_ maybeConfigLogLevel $ \configLogLevel ->
    when (msgLogLevel <= configLogLevel) $ do
      threadId <- embed myThreadId
      E.logAt (fromAwsLogLevel msgLogLevel) $
        "[" <> tshow msgLogLevel <> "] [tid: " <> tshow threadId <> "]"  <> text
  where text = T.decodeUtf8 $ LBS.toStrict message

fromAwsLogLevel :: AWS.LogLevel -> E.LogLevel
fromAwsLogLevel awsLogLevel = case awsLogLevel of
  AWS.Info  -> E.LevelInfo
  AWS.Error -> E.LevelError
  AWS.Debug -> E.LevelDebug
  AWS.Trace -> E.LevelDebug

