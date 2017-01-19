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
