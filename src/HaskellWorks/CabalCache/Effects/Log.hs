{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Log
  ( Log(..)
  , runEffLogConsole
  , LogLevel(..)
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logAt
  ) where

import Data.Text (Text)
import Polysemy

import qualified Data.Text.IO as T

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  deriving (Eq, Show)

data Log m a where
  LogAt     :: LogLevel -> Text -> Log m ()

makeSem ''Log

runEffLogConsole :: Member (Embed IO) r
  => Sem (Log ': r) a
  -> Sem r a
runEffLogConsole = interpret $ \case
  LogAt logLevel s -> case logLevel of
    LevelDebug -> embed $ T.putStrLn ("[Debug] " <> s)
    LevelInfo  -> embed $ T.putStrLn ("[Info] " <> s)
    LevelWarn  -> embed $ T.putStrLn ("[Warn] " <> s)
    LevelError -> embed $ T.putStrLn ("[Error] " <> s)

logDebug :: Member Log r => Text -> Sem r ()
logDebug = logAt LevelDebug
{-# INLINE logDebug #-}

logInfo :: Member Log r => Text -> Sem r ()
logInfo = logAt LevelInfo
{-# INLINE logInfo #-}

logWarn :: Member Log r => Text -> Sem r ()
logWarn = logAt LevelWarn
{-# INLINE logWarn #-}

logError :: Member Log r => Text -> Sem r ()
logError = logAt LevelError
{-# INLINE logError #-}
