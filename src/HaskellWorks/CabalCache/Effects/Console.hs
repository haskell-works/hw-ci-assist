{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Console
  ( Console(..)
  , runEffConsole
  , putStr
  , putStrLn
  , errPutStr
  , errPutStrLn
  ) where

import Data.Text (Text)
import Polysemy
import Prelude   hiding (putStr, putStrLn)

import qualified Data.Text.IO as T
import qualified System.IO    as IO

data Console m a where
  PutStr      :: Text -> Console m ()
  PutStrLn    :: Text -> Console m ()
  ErrPutStr   :: Text -> Console m ()
  ErrPutStrLn :: Text -> Console m ()

makeSem ''Console

runEffConsole :: Member (Embed IO) r
  => Sem (Console ': r) a
  -> Sem r a
runEffConsole = interpret $ \case
  PutStr      s -> embed $ T.putStr   s
  PutStrLn    s -> embed $ T.putStrLn s
  ErrPutStr   s -> embed $ T.hPutStr   IO.stderr s
  ErrPutStrLn s -> embed $ T.hPutStrLn IO.stderr s
