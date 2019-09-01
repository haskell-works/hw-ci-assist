{-# LANGUAGE TemplateHaskell     #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.CabalCache.Effects.Stm
  ( Stm(..)
  , runEffStm
  , atomically
  ) where

import Control.Concurrent.STM (STM)
import Polysemy
import Prelude                hiding (putStr, putStrLn)

import qualified Control.Concurrent.STM as STM

data Stm m a where
  Atomically :: STM a -> Stm m a

makeSem ''Stm

runEffStm :: Member (Embed IO) r
  => Sem (Stm ': r) a
  -> Sem r a
runEffStm = interpret $ \(Atomically f) -> embed $ STM.atomically f
