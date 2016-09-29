{-# LANGUAGE NoImplicitPrelude #-}
module Errors
  ( module X
  , catchM
  ) where

import           Control.Monad.Catch as X (Exception (..), MonadCatch,
                                           MonadThrow (..))
import qualified Control.Monad.Catch as E (catch)

catchM :: (MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
catchM = E.catch
