{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplon.Streaming (streamingLoop, StreamingMessage (..)) where

import Control.Applicative (Applicative (pure))
import Control.Concurrent (Chan, readChan)
import Control.Monad (forever)
import qualified Crypto.Hash as Crypto
import Crypto.Hash.Algorithms (MD5)
import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, encode, object)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (reverse)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (Maybe, maybe)
import Data.Semigroup (Semigroup ((<>)))
import Data.String (String)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import System.IO (IO)
import System.ZMQ4 (Push (Push), bind, sendMulti, withContext, withSocket)
import Text.Show (Show (show))
import Prelude ()

encodeJsonStrict :: (ToJSON a) => a -> ByteString
encodeJsonStrict = BSL.toStrict . encode

data HeaderDetail = All | Basic | None

instance ToJSON HeaderDetail where
  toJSON All = "all"
  toJSON Basic = "basic"
  toJSON None = "none"

data GlobalHeaderPart1 = GlobalHeaderPart1
  { series :: Int,
    headerDetail :: HeaderDetail
  }

instance ToJSON GlobalHeaderPart1 where
  toJSON (GlobalHeaderPart1 {series, headerDetail}) =
    object
      [ "htype" .= ("dheader-1.0" :: Text),
        "series" .= series,
        "header_detail" .= headerDetail
      ]

type GlobalHeaderPart2 = Value

newtype GlobalHeaderPart3 = GlobalHeaderPart3
  { shape :: (Int, Int)
  }

instance ToJSON GlobalHeaderPart3 where
  toJSON (GlobalHeaderPart3 {shape = (shape0, shape1)}) =
    object
      [ "shape" .= [shape0, shape1],
        "htype" .= ("dflatfield-1.0" :: Text),
        "type" .= ("float32" :: Text)
      ]

type GlobalHeaderPart4 = ByteString

newtype GlobalHeaderPart5 = GlobalHeaderPart5
  { shape :: (Int, Int)
  }

instance ToJSON GlobalHeaderPart5 where
  toJSON (GlobalHeaderPart5 {shape = (shape0, shape1)}) =
    object
      [ "shape" .= [shape0, shape1],
        "htype" .= ("dpixelmask-1.0" :: Text),
        "type" .= ("uint32" :: Text)
      ]

type GlobalHeaderPart6 = ByteString

newtype GlobalHeaderPart7 = GlobalHeaderPart7
  { shape :: (Int, Int)
  }

instance ToJSON GlobalHeaderPart7 where
  toJSON (GlobalHeaderPart7 {shape = (shape0, shape1)}) =
    object
      [ "shape" .= [shape0, shape1],
        "htype" .= ("dcountrate_table-1.0" :: Text),
        "type" .= ("float32" :: Text)
      ]

type GlobalHeaderPart8 = ByteString

data ImageDataPart1 = ImageDataPart1
  { series :: Int,
    frame :: Int,
    hash :: Text
  }

instance ToJSON ImageDataPart1 where
  toJSON (ImageDataPart1 {series, frame, hash}) =
    object
      [ "htype" .= ("dimage-1.0" :: Text),
        "series" .= series,
        "frame" .= frame,
        "hash" .= hash
      ]

data ImageDataPart2 = ImageDataPart2
  { -- Either [x,y] or [x,y,z]
    shape :: [Int],
    type_ :: Text,
    encoding :: Text,
    size :: Int
  }

instance ToJSON ImageDataPart2 where
  toJSON (ImageDataPart2 {shape, type_, encoding, size}) =
    object
      [ "htype" .= ("dimage_d-1.0" :: Text),
        "shape" .= shape,
        "type" .= type_,
        "encoding" .= encoding,
        "size" .= size
      ]

type ImageDataPart3 = ByteString

data ImageDataPart4 = ImageDataPart4
  { startTime :: Int,
    stopTime :: Int,
    realTime :: Int
  }

instance ToJSON ImageDataPart4 where
  toJSON (ImageDataPart4 {startTime, stopTime, realTime}) =
    object
      [ "htype" .= ("dconfig-1.0" :: Text),
        "start_time" .= startTime,
        "stop_time" .= stopTime,
        "real_time" .= realTime
      ]

newtype EndOfSeries = EndOfSeries
  { series :: Int
  }

instance ToJSON EndOfSeries where
  toJSON (EndOfSeries {series}) =
    object
      [ "htype" .= ("dseries_end-1.0" :: Text),
        "series" .= series
      ]

data StreamingHeaderData = StreamingHeaderData {series :: Int, appendix :: Maybe Text}

data StreamingImageData = StreamingImageData
  { image :: ByteString,
    imageShape :: [Int],
    series :: Int,
    frame :: Int,
    startTimeNs :: Int,
    stopTimeNs :: Int,
    realTimeNs :: Int,
    appendix :: Maybe Text
  }

newtype StreamingEndOfSeriesData = StreamingEndOfSeriesData {series :: Int}

data StreamingMessage
  = StreamingHeader StreamingHeaderData
  | StreamingImage StreamingImageData
  | StreamingEndOfSeries StreamingEndOfSeriesData

md5HashAsText :: ByteString -> Text
md5HashAsText bs = pack (show (Crypto.hash bs :: Crypto.Digest MD5))

serializeStreamingMessage :: StreamingMessage -> NE.NonEmpty ByteString
serializeStreamingMessage (StreamingHeader (StreamingHeaderData {series, appendix})) =
  encodeJsonStrict (GlobalHeaderPart1 {series = series, headerDetail = None}) NE.:| maybe [] (pure . TE.encodeUtf8) appendix
serializeStreamingMessage (StreamingEndOfSeries (StreamingEndOfSeriesData {series})) =
  NE.singleton $ encodeJsonStrict (EndOfSeries {series = series})
serializeStreamingMessage (StreamingImage (StreamingImageData {image, imageShape, series, frame, startTimeNs, stopTimeNs, realTimeNs, appendix})) =
  let part2 =
        ImageDataPart2
          { -- shape is reversed for some reason
            shape = reverse imageShape,
            type_ = "uint16",
            encoding = "<",
            size = BS.length image
          }
      part2Bytes = encodeJsonStrict part2
      part2Hash = md5HashAsText part2Bytes
      part1Bytes = encodeJsonStrict $ ImageDataPart1 {series = series, frame = frame, hash = part2Hash}
      part3Bytes = image
      part4Bytes = encodeJsonStrict $ ImageDataPart4 {startTime = startTimeNs, stopTime = stopTimeNs, realTime = realTimeNs}
   in part1Bytes NE.:| ([part2Bytes, part3Bytes, part4Bytes] <> maybe [] (pure . TE.encodeUtf8) appendix)

streamingLoop :: (Text -> IO ()) -> String -> Chan StreamingMessage -> IO ()
streamingLoop log bindAddress chan = withContext \context -> forever do
  log "creating ZMQ socket"
  withSocket context Push \socket -> do
    log ("binding to ZMQ " <> pack bindAddress)
    bind socket bindAddress
    log "bind complete, starting wait loop"

    forever do
      newMessage <- readChan chan
      log "got a new message, sending"

      sendMulti socket (serializeStreamingMessage newMessage)
