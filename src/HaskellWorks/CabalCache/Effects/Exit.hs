{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Exit
  ( Exit(..)
  , runEffExit
  , exitWith
  , exitAndPrintOnFail
  , justOrFail
  , successOrFail
  ) where

import Data.Text                     (Text)
import HaskellWorks.CabalCache.Error (ErrorMessage)
import Polysemy

import qualified Data.Text                               as T
import qualified HaskellWorks.CabalCache.Effects.Console as E
import qualified Polysemy.Error                          as E
import qualified Polysemy.Fail                           as E
import qualified System.Exit                             as IO

data Exit m a where
  ExitWith   :: IO.ExitCode -> Exit m ()

makeSem ''Exit

runEffExit :: Member (Embed IO) r
  => Sem (Exit ': r) a
  -> Sem r a
runEffExit = interpret $ \case
  ExitWith    s -> embed $ IO.exitWith   s

exitAndPrintOnFail :: Members
  '[Embed IO
  , E.Console
  ] r
  => Sem (E.Fail ': r) a -> Sem r a
exitAndPrintOnFail f = do
  result <- E.runFail f
  case result of
    Right a -> return a
    Left msg -> do
      E.errPutStrLn (T.pack msg)
      runEffExit (exitWith (IO.ExitFailure 1))
      error msg

justOrFail :: Member E.Fail r
  => Text -> Maybe a -> Sem r a
justOrFail msg mv = case mv of
  Just v  -> return v
  Nothing -> fail (T.unpack msg)

successOrFail :: forall e r a. (Member (E.Error e) r, ErrorMessage e)
  => Sem r a
  -> Sem r a
successOrFail f = E.catch @e f undefined
