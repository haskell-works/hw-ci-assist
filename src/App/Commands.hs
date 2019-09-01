module App.Commands where

import App.Commands.PolyVersion
import App.Commands.SyncFromArchive
import App.Commands.SyncToArchive
import App.Commands.Version
import Data.Semigroup               ((<>))
import Options.Applicative

{-# ANN module ("HLint: ignore Monoid law, left identity"  :: String) #-}

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdSyncFromArchive
  <>  cmdSyncToArchive
  <>  cmdVersion
  <>  cmdPolyVersion
