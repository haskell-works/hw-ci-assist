{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.PolyVersion
  ( cmdPolyVersion
  ) where

import App.Commands.Options.Parser (optsVersion)
import Data.List
import Control.Lens ((&))
import Data.Semigroup              ((<>))
import Options.Applicative         hiding (columns)
import Polysemy

import qualified App.Commands.Options.Types         as Z
import qualified Data.Text                          as T
import qualified Data.Version                       as V
import qualified Paths_cabal_cache                  as P
import qualified HaskellWorks.CabalCache.Effects.Console as E

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runPolyVersion :: Z.VersionOptions -> IO ()
runPolyVersion opts = runInEffect opts
  & E.runEffConsole
  & runM

runInEffect :: Members '[E.Console] r
  => Z.VersionOptions -> Sem r ()
runInEffect _ = do
  let V.Version {..} = P.version
  let version = intercalate "." $ fmap show versionBranch
  E.putStr $ "cabal-cache " <> T.pack version


cmdPolyVersion :: Mod CommandFields (IO ())
cmdPolyVersion = command "poly-version"  $ flip info idm $ runPolyVersion <$> optsVersion
