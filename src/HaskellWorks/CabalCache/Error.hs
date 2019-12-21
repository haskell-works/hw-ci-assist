module HaskellWorks.CabalCache.Error
  ( nothingToError
  , ErrorMessage (..)
  ) where

import Control.Monad.Except
import Data.Text            (Text)

nothingToError :: MonadError e m => e -> Maybe a -> m a
nothingToError _ (Just a) = return a
nothingToError e Nothing  = throwError e

class ErrorMessage a where
  errorMessage :: a -> Text
