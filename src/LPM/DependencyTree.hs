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
  , latestOrFail)
import Control.Monad.IO.Class
import Control.Monad.State
import Prelude hiding (lookup)
import Control.Monad.Except

data Node = Node String String PackageDist

instance Show Node where
  show (Node name version _) = name ++ "@" ++ version

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

buildDependencyTree :: (MonadError String m, MonadIO m, MonadState (Map String PackageInfo) m) => PackageInfo -> m DependencyTree
buildDependencyTree packageInfo = do
  package <- latestOrFail packageInfo
  let node = nodeFromPackage package
  dependencyInfoMap <- traverseWithKey (\k _ -> getFromCacheOrFetch k) (dependencies package)
  case ((<$>) snd . toList) dependencyInfoMap of
    [] -> pure $ DependencyTree node []
    pkgs -> DependencyTree node <$> traverse buildDependencyTree pkgs
  where
    getFromCacheOrFetch s = getFromCache s >>= \case
      Nothing -> do
        packageInfo <- getPackageInfo s
        addToCache s packageInfo
        return packageInfo
      Just packageInfo -> pure packageInfo
