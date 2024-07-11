{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Simplon.Options (Options (..), withOptions) where

import Control.Applicative ((<*>))
import Data.Bool (Bool)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text, pack)
import qualified Options.Applicative as Opt
import System.IO (FilePath, IO)
import Prelude ()

data Options = Options
  { listenPort :: Int,
    inputH5File :: FilePath,
    inputH5DatasetPath :: Text,
    accessLogging :: Bool
  }

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> Opt.option
      Opt.auto
      ( Opt.long "listen"
          <> Opt.help "port to listen with the simplon web server"
          <> Opt.showDefault
          <> Opt.value 8080
      )
    <*> Opt.strOption
      ( Opt.long "input-h5-file"
          <> Opt.help "which file to write to ZeroMQ (has to be three-dimensional and have a data set H5_DATASET)"
      )
    <*> Opt.strOption
      ( Opt.long "input-h5-dataset"
          <> Opt.help "in which dataset path to search for images"
          <> Opt.value (pack "/entry/data/data")
          <> Opt.showDefault
          <> Opt.metavar "H5_DATASET"
      )
    <*> Opt.switch
      ( Opt.long "access-logging" <> Opt.help "enable logging of every incoming request (tends to be spammy)"
      )

withOptions :: (Options -> IO a) -> IO a
withOptions f = do
  parsedOpts <- Opt.execParser opts
  f parsedOpts
  where
    opts =
      Opt.info
        (optionsParser Opt.<**> Opt.helper)
        ( Opt.fullDesc <> Opt.progDesc "Run a web server that mimicks the Eiger detector's Simplon REST interface"
        )
