{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Either
  ( maybeToLeft
  , maybeToRight
  , leftToMaybe
  , rightToMaybe
  , maybeToEither
  , whenIsRight
  , whenIsLeft
  , whenRightDo
  , whenLeftDo
  ) where

import           Control.Applicative (Applicative (pure))
import           Data.Either   (Either (..), either)
import           Data.Function (const)
import           Data.Maybe    (Maybe (..), maybe)
import           Data.Monoid   (Monoid, mempty)

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToEither :: Monoid b => (a -> b) -> Maybe a -> b
maybeToEither = maybe mempty

whenIsRight :: (Applicative m) => Either l r -> m () -> m ()
whenIsRight (Right _) f = f
whenIsRight (Left _)  _ = pure ()

whenIsLeft :: (Applicative m) => Either l r -> m () -> m ()
whenIsLeft (Left _)  f = f
whenIsLeft (Right _) _ = pure ()

whenRightDo :: (Applicative m) => Either l r -> (r -> m ()) -> m ()
whenRightDo (Right r) f = f r
whenRightDo (Left _)  _ = pure ()

whenLeftDo :: (Applicative m) => Either l r -> (l -> m ()) -> m ()
whenLeftDo (Left l)  f = f l
whenLeftDo (Right _) _ = pure ()
