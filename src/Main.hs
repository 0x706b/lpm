{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad.Except
import Control.Monad.State (MonadState, evalStateT)
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (String)
import GHC.Base (IO)
import LPM.DependencyTree (DependencyTree, buildDependencyTree)
import LPM.Info (PackageInfo, getPackageInfo)
import System.IO (getLine, print)
import Data.Maybe

program :: (MonadError String m, MonadIO m, MonadState (Map String PackageInfo) m) => m DependencyTree
program = do
  packageName <- liftIO getLine
  getPackageInfo packageName >>= buildDependencyTree Nothing

main :: IO ()
main = do
  result <- runExceptT (evalStateT program Map.empty)
  case result of
    Left err -> print err
    Right dependencyTree -> print dependencyTree
