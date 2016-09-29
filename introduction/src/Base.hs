{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Base
    ( module X
    ) where

import GHC.Num as X
import GHC.Enum as X
import GHC.Real as X
import GHC.Float as X
import GHC.Show as X (
      Show(..)
    )

import GHC.Base as X (
      (++)
    , seq
    , asTypeOf
    , ord
    )

import System.IO as X (
      print
    , putStr
    , putStrLn
    , writeFile
    , readFile
    , appendFile
    )

import GHC.Types as X (
      Bool
    , Char
    , Int
    , Word
    , Ordering
    , IO
    , Coercible
    )

import Data.Kind as X (
      type (*)
    , type Type
    )
