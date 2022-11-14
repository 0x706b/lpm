{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module LPM.DependencyTree
  ( DependencyTree
  , buildDependencyTree
  )
where

import Data.Map
import Data.Maybe hiding (mapMaybe)
import LPM.Info
  ( Package (dependencies, dist, name, version)
  , PackageDist
  , PackageInfo
  , getPackageInfo
  , latestOrFail, resolvePackage)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Except
import Data.SemVer (Version, toString)
import Data.SemVer.Constraint (Constraint)
import Prelude hiding (lookup)

data Node = Node
  String
  -- ^ The name of the package
  Version
  -- ^ The semver version of the package
  PackageDist
  -- ^ The distribution info of the package

instance Show Node where
  show (Node name version _) = name ++ "@" ++ toString version

data DependencyTree = DependencyTree Node [DependencyTree]

instance Show DependencyTree where
  show = go 0
    where
      go depth (DependencyTree node []) = "\n" ++ replicate depth ' ' ++ show node
      go depth (DependencyTree node tree) =
        "\n"
          ++ replicate depth ' '
          ++ show node
          ++ (go (depth + 1) =<< tree)

nodeFromPackage :: Package -> Node
nodeFromPackage p = Node (name p) (version p) (dist p)

addToCache :: (MonadState (Map String PackageInfo) m) => String -> PackageInfo -> m ()
addToCache id packageInfo = modify $ insert id packageInfo

getFromCache :: (MonadState (Map String PackageInfo) m) => String -> m (Maybe PackageInfo)
getFromCache id = gets $ lookup id

buildDependencyTree :: (MonadError String m, MonadIO m, MonadState (Map String PackageInfo) m) => Maybe Constraint -> PackageInfo -> m DependencyTree
buildDependencyTree constraint packageInfo = do
  package <- case constraint of 
    Nothing -> latestOrFail packageInfo
    Just c -> case resolvePackage c packageInfo of
      Nothing -> latestOrFail packageInfo
      Just p -> pure p
  let node = nodeFromPackage package
  dependencyInfoMap <- traverseWithKey (\k c -> (,) c <$> getFromCacheOrFetch k) (dependencies package)
  case ((<$>) snd . toList) dependencyInfoMap of
    []   -> pure $ DependencyTree node []
    pkgs -> DependencyTree node <$> traverse (\(c, info) -> buildDependencyTree (Just c) info) pkgs
  where
    getFromCacheOrFetch s = getFromCache s >>= \case
      Nothing -> do
        packageInfo <- getPackageInfo s
        addToCache s packageInfo
        return packageInfo
      Just packageInfo -> pure packageInfo
