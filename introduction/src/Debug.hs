{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe            #-}

module Debug
  ( undefined
  , error
  , trace
  , traceM
  , traceIO
  , traceShow
  , traceShowId
  , traceShowM
  , notImplemented
  ) where

import           Control.Monad (Monad, return)
import           Data.String   (String)

import           Base          as P
import qualified Debug.Trace   as T
import qualified GHC.Err       as P (error, undefined)

{-# WARNING undefined "'undefined' remains in code" #-}
undefined :: a
undefined = P.undefined

{-# WARNING error "'error' remains in code" #-}
error :: String -> a
error = P.error

{-# WARNING trace "'trace' remains in code" #-}
trace :: String -> a -> a
trace = T.trace

{-# WARNING traceShow "'traceShow' remains in code" #-}
traceShow :: P.Show a => a -> b -> b
traceShow a b = T.trace (P.show a) b

{-# WARNING traceShowM "'traceShowM' remains in code" #-}
traceShowM :: (P.Show a, Monad m) => a -> m ()
traceShowM a = traceM (P.show a)

{-# WARNING traceShowId "'traceShowId' remains in code" #-}
traceShowId :: P.Show a => a -> a
traceShowId a = T.traceShowId a

{-# WARNING traceM "'traceM' remains in code" #-}
traceM :: (Monad m) => String -> m ()
traceM s = T.trace s (return ())

{-# WARNING traceIO "'traceIO' remains in code" #-}
traceIO :: String -> P.IO ()
traceIO = T.traceIO

{-# WARNING notImplemented "'notImplemented' remains in code" #-}
notImplemented :: a
notImplemented = P.error "Not implemented"
