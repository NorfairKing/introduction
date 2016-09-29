{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Monad (
    Monad((>>=), return)
  , MonadPlus(..)

  , (=<<)
  , (>=>)
  , (<=<)
  , forever

  , join
  , mfilter
  , filterM
  , mapAndUnzipM
  , zipWithM
  , zipWithM_
  , foldM
  , foldM_
  , replicateM
  , replicateM_
  , concatMapM

  , guard
  , when
  , unless

  , liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  , ap
  ) where

import           Data.List     (concat)

import           Control.Monad hiding ((<$!>))

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
