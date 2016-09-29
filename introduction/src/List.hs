{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module List
    ( ordNub
    , sortOn
    , uncons
    , snoc
    , windows
    , chunksOf
    ) where

import           Data.Bool     (otherwise)
import           Data.Function ((.))
import           Data.Int      (Int)
import           Data.List     (drop, length, sortBy, take, (++))
import           Data.Maybe    (Maybe (..))
import           Data.Ord      (Ord (..), comparing)
import qualified Data.Set      as Set

sortOn :: (Ord o) => (a -> o) -> [a] -> [a]
sortOn = sortBy . comparing

-- | Fast @nub@ if the contents of the list are orderable
--
-- O(n * log n)
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ []     = []
    go s (x:xs) =
      if x `Set.member` s
      then go s xs
      else x : go (Set.insert x s) xs

-- | Deconstruct a list into its first element the rest of the list
-- This will evaluate to @Nothing@ if the list is empty.
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)

-- | @(:)@, appending instead of prepending
-- @(:)@ is sometimes called cons, so when we append instead of prepend, we call the function @snoc@
-- Note that this has an O(length list) time complexity
snoc :: [a] -> a -> [a]
snoc as a = as ++ [a]

-- | Find all @i@-sized windows in a list
windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows i xss@(_:xs)
    | length xss >= i = (take i xss) : windows i xs
    | otherwise       = []

-- | Takes chunks of a given size, the last chunk may be smaller but not empty.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs
    | length xs >= i = take i xs : chunksOf i (drop i xs)
    | otherwise = [xs]
