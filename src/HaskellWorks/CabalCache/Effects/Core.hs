{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module HaskellWorks.CabalCache.Effects.Core
  ( mkCompilerContext
  ) where

import Control.Monad.Except
import Data.Text            (Text)
import Polysemy
import Polysemy.Error

import qualified HaskellWorks.CabalCache.Core  as C
import qualified HaskellWorks.CabalCache.Types as Z

mkCompilerContext :: Members [Embed IO, Error Text] r
  => Z.PlanJson
  -> Sem r Z.CompilerContext
mkCompilerContext planJson = do
  result <- embed $ (runExceptT (C.mkCompilerContext planJson) :: IO (Either Text Z.CompilerContext))
  case result of
    Right cc -> return cc
    Left e   -> throw e
