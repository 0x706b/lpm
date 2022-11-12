{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveAnyClass #-}

module LPM.Package where

import Data.String
import Data.Maybe
import Text.Show
import Data.Map
import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import GHC.Generics

data Package = Package {
  name             :: String
, version          :: Maybe String
, license          :: String
, workspaces       :: Maybe [String]
, scripts          :: Map String String
, devDependencies  :: Maybe (Map String String)
, dependencies     :: Maybe (Map String String)
, peerDependencies :: Maybe (Map String String)
, resolutions      :: Maybe (Map String String)
} deriving (Generic, Show, ToJSON, FromJSON)

decodePackageJson :: ByteString -> Maybe Package
decodePackageJson = decode
