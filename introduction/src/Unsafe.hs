{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe            #-}

module Unsafe (
  unsafeHead,
  unsafeTail,
  unsafeInit,
  unsafeLast,
  unsafeFromJust,
  unsafeIndex,
) where

import           Base       (Int)
import qualified Data.List  as List
import qualified Data.Maybe as Maybe

{-# WARNING unsafeHead "An unsafe function: 'unsafehead' remains in code" #-}
unsafeHead :: [a] -> a
unsafeHead = List.head

{-# WARNING unsafeTail "An unsafe function: 'unsafeTail' remains in code" #-}
unsafeTail :: [a] -> [a]
unsafeTail = List.tail

{-# WARNING unsafeInit "An unsafe function: 'unsafeInit' remains in code" #-}
unsafeInit :: [a] -> [a]
unsafeInit = List.init

{-# WARNING unsafeLast "An unsafe function: 'unsafeLast' remains in code" #-}
unsafeLast :: [a] -> a
unsafeLast = List.last

{-# WARNING unsafeFromJust "An unsafe function: 'unsafeFromJust' remains in code" #-}
unsafeFromJust :: Maybe.Maybe a -> a
unsafeFromJust = Maybe.fromJust

{-# WARNING unsafeIndex "An unsafe function: 'unsafeIndex' remains in code" #-}
unsafeIndex :: [a] -> Int -> a
unsafeIndex = (List.!!)
