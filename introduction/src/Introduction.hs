{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE Trustworthy           #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Introduction
  ( module X
  , module Base
  , applyN
  , show

  , SText
  , LText
  , SByteString
  , LByteString
  ) where

import           Bool                      as X
import           Concurrency               as X
import           Debug                     as X
import           Either                    as X
import           Errors                    as X
import           FilePaths                 as X
import           Functor                   as X
import           IOString                  as X
import           List                      as X
import           Monad                     as X

import           Base                      as Base hiding (appendFile, putStr,
                                                    putStrLn, readFile, show,
                                                    writeFile)

import           Data.String               as X (String)

-- Maybe'ized version of partial functions
import           Safe                      as X (atDef, atMay, foldl1May,
                                                 foldr1May, headDef, headMay,
                                                 initDef, initMay, initSafe,
                                                 lastDef, lastMay, tailDef,
                                                 tailMay, tailSafe)

-- Applicatives
import           Control.Applicative       as X (Alternative (..),
                                                 Applicative (..), Const (..),
                                                 ZipList (..), liftA, liftA2,
                                                 liftA3, optional, (<**>))

-- Base typeclasses
import           Data.Data                 as X (Data (..))
import           Data.Eq                   as X (Eq (..))
import           Data.Foldable             as X hiding (foldl1, foldr1)
import           Data.Functor.Identity     as X
import           Data.Monoid               as X
import           Data.Ord                  as X
import           Data.Traversable          as X

import           Data.RelativeValidity     as X
import           Data.Validity             as X
import           Data.Validity.ByteString  as X ()
import           Data.Validity.Containers  as X ()
import           Data.Validity.Text        as X ()

-- Deepseq
import           Control.DeepSeq           as X (NFData (..), deepseq, force,
                                                 ($!!))

-- Data structures
import           Data.List                 as X (break, cycle, drop, dropWhile,
                                                 filter, group, inits,
                                                 intercalate, intersperse,
                                                 isPrefixOf, iterate, map, map,
                                                 permutations, repeat,
                                                 replicate, reverse, scanl,
                                                 scanr, sort, sortBy, splitAt,
                                                 subsequences, tails, take,
                                                 takeWhile, transpose, unfoldr,
                                                 zip, zipWith)
import           Data.Tuple                as X

import           Data.IntMap               as X (IntMap)
import           Data.IntSet               as X (IntSet)
import           Data.Map                  as X (Map)
import           Data.Sequence             as X (Seq)
import           Data.Set                  as X (Set)

import           Data.Proxy                as X (Proxy (..))

import           Data.Typeable             as X (TypeRep, Typeable, cast, eqT,
                                                 typeRep)

import           Data.Type.Coercion        as X (Coercion (..), coerceWith)

import           Data.Type.Equality        as X ((:~:) (..), type (==),
                                                 castWith, gcastWith, sym,
                                                 trans)

import           Data.Void                 as X (Void, absurd, vacuous)

-- Monad transformers
import           Control.Monad.State       as X (MonadState, State, StateT,
                                                 evalState, evalStateT,
                                                 execState, execStateT, get,
                                                 gets, modify, put, runState,
                                                 runStateT, withState)

import           Control.Monad.Reader      as X (MonadReader, Reader, ReaderT,
                                                 ask, asks, local, runReader,
                                                 runReaderT)

import           Control.Monad.Writer      as X (MonadWriter, Writer, WriterT,
                                                 execWriter, execWriterT,
                                                 listen, pass, runWriter,
                                                 runWriterT, tell, writer)

import           Control.Monad.Except      as X (Except, ExceptT, MonadError,
                                                 catchError, mapExcept,
                                                 mapExceptT, runExcept,
                                                 runExceptT, throwError,
                                                 withExceptT)

import           Control.Monad.Base        as X (MonadBase)
import           Control.Monad.IO.Class    as X (MonadIO, liftIO)
import           Control.Monad.Trans.Class as X (lift)

-- Base types
import           Data.Bits                 as X
import           Data.Bool                 as X hiding (bool)
import           Data.Char                 as X (chr)
import           Data.Complex              as X
import           Data.Either               as X
import           Data.Int                  as X
import           Data.Maybe                as X hiding (fromJust)
import           Data.Word                 as X

import           Data.Function             as X (const, fix, flip, id, on, ($),
                                                 (.))

-- Genericss
import           GHC.Generics              as X (Generic)

-- ByteString
import           Data.ByteString           as X (ByteString)
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- Text
import           Data.Text                 as X (Text)
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import           Text.Show                 (Show (..))

import           Data.Text.Lazy            (fromStrict, toStrict)

import           Data.String               as X (IsString)

-- Printf
import           Text.Printf               as X (PrintfArg, hPrintf, printf)

-- IO
import           System.Exit               as X
--import System.Info as X
import           System.Environment        as X (getArgs)
import           System.IO                 as X (Handle)

-- ST
import           Control.Monad.ST          as X

import           Foreign.Storable          as X (Storable)

-- Read instances hiding unsafe builtins (read)
import           Text.Read                 as X (Read, readEither, readMaybe,
                                                 reads)

-- Type synonymss
type SText = Data.Text.Text
type LText = Data.Text.Lazy.Text

type SByteString = Data.ByteString.ByteString
type LByteString = Data.ByteString.Lazy.ByteString

applyN :: Int -> (a -> a) -> a -> a
applyN n f = X.foldr (.) id (X.replicate n f)
