{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}

module LPM.Info where

import Data.String
import Data.Maybe
import Data.Functor
import Data.Monoid
import Text.Show
import Data.Map hiding (foldr)
import Data.Foldable
import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import GHC.Base hiding (Constraint, foldr)
import GHC.Generics
import Network.HTTP.Simple
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Aeson.Types (prependFailure, unexpected)
import Data.Either
import qualified Data.SemVer
import qualified Data.SemVer.Constraint as C
import Data.Functor.Reverse

instance FromJSON Data.SemVer.Version where
  parseJSON = withText "Version" $ \t -> case Data.SemVer.fromText t of
    Left err -> prependFailure err $ unexpected (String t)
    Right a  -> pure a

instance FromJSONKey Data.SemVer.Version where
  fromJSONKey = FromJSONKeyTextParser (\t ->
    case Data.SemVer.fromText t of
      Left err -> prependFailure err $ unexpected (String t)
      Right a  -> pure a
    )

instance ToJSON Data.SemVer.Version where
  toJSON = String . Data.SemVer.toText

instance FromJSON C.Constraint where
  parseJSON = withText "Constraint" $ \t -> case C.fromText t of
    Left err -> prependFailure err $ unexpected (String t)
    Right a  -> pure a

data PackageDist = PackageDist {
  integrity :: String
, shasum    :: String
, tarball   :: String
} deriving (Generic, Show, ToJSON, FromJSON)

data Package = Package {
  name             :: String
, version          :: Data.SemVer.Version
, license          :: Maybe String
, dependencies     :: Map String C.Constraint
, peerDependencies :: Map String C.Constraint
, dist             :: PackageDist
} deriving (Show)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \v -> Package
    <$> v .: "name"
    <*> v .: "version"
    <*> v .:? "license"
    <*> (fromMaybe Map.empty <$> v .:? "dependencies")
    <*> (fromMaybe Map.empty <$> v .:? "peerDependencies")
    <*> v .: "dist"

data PackageInfo = PackageInfo {
  name      :: String
, dist_tags :: Map String Data.SemVer.Version
, versions  :: Map Data.SemVer.Version Package
} deriving (Show)

instance FromJSON PackageInfo where
  parseJSON = withObject "PackageInfo" $ \v -> PackageInfo
    <$> v .: "name"
    <*> v .: "dist-tags"
    <*> (fromMaybe Map.empty <$> v .:? "versions")

latestVersion :: PackageInfo -> Data.SemVer.Version
latestVersion packageInfo = dist_tags packageInfo ! "latest"

latest :: PackageInfo -> Maybe Package
latest packageInfo = lookup (latestVersion packageInfo) (versions packageInfo)

latestOrFail :: (MonadError String m) => PackageInfo -> m Package
latestOrFail packageInfo = case latest packageInfo of
  Nothing        -> throwError "No package found"
  (Just package) -> pure package

decodePackageInfo :: (MonadError String m) => ByteString -> m PackageInfo
decodePackageInfo = liftEither . eitherDecode

getPackageInfo :: (MonadError String m, MonadIO m) => String -> m PackageInfo
getPackageInfo packageName = do
  response <- httpLBS $ parseRequest_ ("https://registry.npmjs.org/" <> packageName)
  (decodePackageInfo . getResponseBody) response

resolvePackage :: C.Constraint -> PackageInfo -> Maybe Package
resolvePackage v p = foldr
  ( \a b ->
    if C.satisfies (version a) v
    then Just a
    else b
  )
  Nothing
  (Reverse (versions p))
