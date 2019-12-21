{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module HaskellWorks.CabalCache.Effects.Plan
  ( PlanError(..)
  , loadPlan
  ) where

import Data.Aeson
import Data.Text                     (Text)
import HaskellWorks.CabalCache.Error
import HaskellWorks.CabalCache.Show
import Polysemy
import Polysemy.Error
import Prelude                       hiding (init)
import System.FilePath               ((</>))

import qualified Data.Text                                  as T
import qualified HaskellWorks.CabalCache.Effects.FileSystem as E
import qualified HaskellWorks.CabalCache.Types              as Z

newtype PlanError = PlanExtractError Text
  deriving (Eq, Show)

instance ErrorMessage PlanError where
  errorMessage (PlanExtractError exitFailureCode) = "Failed to extract Plan.  Exit code: " <> tshow exitFailureCode

loadPlan ::
    Members
      [ E.FileSystem
      , Error PlanError
      ] r
  => Sem r Z.PlanJson
loadPlan = do
  lbs <- E.readFile ("dist-newstyle" </> "cache" </> "plan.json")
  case eitherDecode lbs of
    Right planJson -> return planJson
    Left errorMsg  -> throw (PlanExtractError (T.pack errorMsg))
