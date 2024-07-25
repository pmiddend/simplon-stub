module Simplon.Util (packShow, packShowLazy, fini, overwriteMVar) where

import Control.Applicative (Applicative (pure))
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (STM, TVar, atomically, check, readTVar, registerDelay)
import Control.Monad ((<=<))
import Data.Bool (Bool)
import Data.Function (const, (.))
import qualified Data.Text.Lazy as TL
import System.IO (IO)
import System.Log.FastLogger (LogStr, LogType' (LogStdout), ToLogStr (toLogStr), defaultBufSize, newTimeCache, withTimedFastLogger)
import Text.Show (Show (show))
import Prelude ()

packShow :: (Show a) => a -> LogStr
packShow = toLogStr . show

packShowLazy :: (Show a) => a -> TL.Text
packShowLazy = TL.pack . show

fini :: TVar Bool -> STM ()
fini = check <=< readTVar

overwriteMVar :: MVar a -> a -> IO ()
overwriteMVar var value = modifyMVar_ var (const (pure value))
