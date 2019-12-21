{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module HaskellWorks.CabalCache.Effects.Store where

import Antiope.Core         (runAws, runResAws)
import Antiope.Env          (HasEnv)
import Antiope.S3           (BucketName (..), ObjectKey (..), S3Uri (..))
import Control.Monad        (void)
import Control.Resource     (ReleaseToken, release, unpackResourceT)
import Data.ByteString.Lazy (ByteString)
import Data.Function        ((&))
import Data.Functor         ((<&>))
import Network.URI          (URI)
import Polysemy
import Polysemy.Error
import Polysemy.Resource

import qualified Antiope.S3      as S3
import qualified Antiope.S3.Lazy as S3
import qualified Data.Text       as Text
import qualified Network.URI     as URI

data Store m a where
  ReadContent   :: URI -> Store m (ReleaseToken, Maybe ByteString)
  Exists        :: URI -> Store m Bool
  WriteContent  :: URI -> ByteString -> Store m ()
  Close         :: ReleaseToken -> Store m ()

makeSem ''Store

newtype StoreError = InvalidUri URI
  deriving (Show, Eq, Ord)

runStoreS3 :: (HasEnv env, Member (Embed IO) r, Member (Error StoreError) r)
  => env
  -> Sem (Store ': r) a
  -> Sem r a
runStoreS3 e = interpret $ \case
  ReadContent u     -> toS3Uri u >>= (embed . unpackResourceT . runAws e . S3.downloadFromS3Uri)
  WriteContent u c  -> toS3Uri u >>= (embed . void . runResAws e . flip S3.putContent' c)
  Exists u          -> toS3Uri u >>= (embed . runResAws e . S3.fileExists)
  Close t           -> embed $ release t

toS3Uri :: Member (Error StoreError) r => URI -> Sem r S3Uri
toS3Uri uri = maybe (throw (InvalidUri uri)) pure (toS3Uri' uri)

toS3Uri' :: URI -> Maybe S3Uri
toS3Uri' uri = S3Uri <$> bkt <*> pure key
  where bkt = uri & URI.uriAuthority <&> URI.uriRegName <&> Text.pack <&> BucketName
        key = uri & URI.uriPath & drop 1 & Text.pack & ObjectKey

withStoreContent :: Members '[Store, Resource] r
  => URI
  -> (Maybe ByteString -> Sem r a)
  -> Sem r a
withStoreContent uri f = bracket (readContent uri) (close . fst) (f . snd)
