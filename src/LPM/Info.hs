{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module LPM.Info where

import Data.String
import Data.Maybe
import Data.Functor
import Data.Monoid
import Text.Show
import Data.Map
import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import GHC.Base
import GHC.Generics
import Network.HTTP.Simple
import Control.Monad.Except
import qualified Data.Map as Map

data PackageDist = PackageDist {
  integrity :: String
, shasum    :: String
, tarball   :: String
} deriving (Generic, Show, ToJSON, FromJSON)

data Package = Package {
  name             :: String
, version          :: String
, license          :: Maybe String
, dependencies     :: Map String String
, peerDependencies :: Map String String
, dist             :: PackageDist
} deriving (Generic, Show, ToJSON)

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
, dist_tags :: Map String String
, versions  :: Maybe (Map String Package)
} deriving (Generic, Show)

instance ToJSON PackageInfo where
  toJSON (PackageInfo name dist_tags versions) =
    object ["name" .= name, "dist-tags" .= dist_tags, "versions" .= versions]

instance FromJSON PackageInfo where
  parseJSON = withObject "PackageInfo" $ \v -> PackageInfo
    <$> v .: "name"
    <*> v .: "dist-tags"
    <*> v .: "versions"
    
latestVersion :: PackageInfo -> String
latestVersion packageInfo = dist_tags packageInfo ! "latest"

latest :: PackageInfo -> Maybe Package
latest packageInfo = lookup (latestVersion packageInfo) =<< versions packageInfo

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
