module HaskellWorks.CabalCache.Error
  ( ErrorMessage (..)
  ) where

import Data.Text (Text)

class ErrorMessage a where
  errorMessage :: a -> Text
