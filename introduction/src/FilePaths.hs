{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module FilePaths
    ( module X
    ) where

import           Data.Bool       (not, (&&))
import           Data.Eq         ((/=), (==))
import           Data.List       (isInfixOf, null)
import           Data.Maybe
import qualified System.FilePath as FilePath

import           Data.Validity

import           Path            as X
import           Path.Internal
import           Path.IO         as X

instance Validity (Path Abs File) where
  isValid p@(Path fp)
    =  FilePath.isAbsolute fp
    && not (FilePath.hasTrailingPathSeparator fp)
    && FilePath.isValid fp
    && not (".." `isInfixOf` fp)
    && (parseAbsFile fp == Just p)

instance Validity (Path Rel File) where
  isValid p@(Path fp)
    =  FilePath.isRelative fp
    && not (FilePath.hasTrailingPathSeparator fp)
    && FilePath.isValid fp
    && fp /= "."
    && fp /= ".."
    && not (".." `isInfixOf` fp)
    && (parseRelFile fp == Just p)

instance Validity (Path Abs Dir) where
  isValid p@(Path fp)
    =  FilePath.isAbsolute fp
    && FilePath.hasTrailingPathSeparator fp
    && FilePath.isValid fp
    && not (".." `isInfixOf` fp)
    && (parseAbsDir fp == Just p)

instance Validity (Path Rel Dir) where
  isValid p@(Path fp)
    =  FilePath.isRelative fp
    && FilePath.hasTrailingPathSeparator fp
    && FilePath.isValid fp
    && not (null fp)
    && fp /= "."
    && fp /= ".."
    && not (".." `isInfixOf` fp)
    && (parseRelDir fp == Just p)

