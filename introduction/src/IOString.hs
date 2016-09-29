{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IOString
    ( IOString
    , writeFile
    , readFile
    , appendFile
    , putStr
    , putStrLn
    , putText
    , putTextLn
    , putLText
    , putLTextLn
    , FilePath
    ) where

import qualified Base

import           Data.Function              ((.), ($))
import           GHC.IO                     (FilePath)

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.IO          as TL

import           Path

class IOString s where
    iosWriteFile  :: FilePath -> s -> Base.IO ()
    iosReadFile   :: FilePath -> Base.IO s
    iosAppendFile :: FilePath -> s -> Base.IO ()
    iosPutStr     :: s -> Base.IO ()
    iosPutStrLn   :: s -> Base.IO ()

instance IOString T.Text where
    iosWriteFile   = T.writeFile
    iosReadFile    = T.readFile
    iosAppendFile  = T.appendFile
    iosPutStr      = T.putStr
    iosPutStrLn    = T.putStrLn

instance IOString TL.Text where
    iosWriteFile   = TL.writeFile
    iosReadFile    = TL.readFile
    iosAppendFile  = TL.appendFile
    iosPutStr      = TL.putStr
    iosPutStrLn    = TL.putStrLn

instance IOString BS.ByteString where
    iosWriteFile   = BS.writeFile
    iosReadFile    = BS.readFile
    iosAppendFile  = BS.appendFile
    iosPutStr      = BS.putStr
    iosPutStrLn    = BS.putStrLn

instance IOString BL.ByteString where
    iosWriteFile   = BL.writeFile
    iosReadFile    = BL.readFile
    iosAppendFile  = BL.appendFile
    iosPutStr      = BL.putStr
    iosPutStrLn    = BL.putStrLn

instance IOString [Base.Char] where
    iosWriteFile   = Base.writeFile
    iosReadFile    = Base.readFile
    iosAppendFile  = Base.appendFile
    iosPutStr      = Base.putStr
    iosPutStrLn    = Base.putStrLn

writeFile :: (IOString s, MonadIO m) => (Path Abs File) -> s -> m ()
writeFile p s = liftIO $ iosWriteFile (toFilePath p) s

readFile :: (IOString s, MonadIO m) => (Path Abs File) -> m s
readFile = liftIO . iosReadFile . toFilePath

appendFile :: (IOString s, MonadIO m) => (Path Abs File) -> s -> m ()
appendFile p s = liftIO $ iosAppendFile (toFilePath p) s

putStr :: (IOString s, MonadIO m) => s -> m ()
putStr = liftIO . iosPutStr

putStrLn :: (IOString s, MonadIO m) => s -> m ()
putStrLn = liftIO . iosPutStrLn

-- For forcing type inference
putText :: MonadIO m => T.Text -> m ()
putText = putStr
{-# SPECIALIZE putText :: T.Text -> Base.IO () #-}

putTextLn :: MonadIO m => T.Text -> m ()
putTextLn = putStrLn
{-# SPECIALIZE putText :: T.Text -> Base.IO () #-}

putLText :: MonadIO m => TL.Text -> m ()
putLText = putStr
{-# SPECIALIZE putLText :: TL.Text -> Base.IO () #-}

putLTextLn :: MonadIO m => TL.Text -> m ()
putLTextLn = putStrLn
{-# SPECIALIZE putLText :: TL.Text -> Base.IO () #-}



