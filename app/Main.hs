{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative (Alternative ((<|>)), Applicative (pure, (<*>)))
import Control.Concurrent (Chan, forkIO, newChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (atomically, registerDelay)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON,
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Number, String),
    object,
    parseJSON,
    withObject,
    (.:),
  )
import Data.Bool (Bool, not)
import qualified Data.ByteString as BS
import Data.Foldable (Foldable (null), for_)
import Data.Function (const, ($))
import Data.Functor (Functor ((<$)), (<$>))
import Data.Int (Int)
import Data.List ((!!))
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Ord (Ord ((>)))
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import Network.HTTP.Types.Status (status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Simplon.Hdf5 (Hdf5DataSet, getDataSetDimensions, withHdf5FileAndDataSet, withImage)
import Simplon.Options (accessLogging, inputH5DatasetPath, inputH5File, listenPort, withOptions, zmqBindAddress)
import Simplon.SeriesId (SeriesId (SeriesId))
import Simplon.Streaming (StreamingEndOfSeriesData (StreamingEndOfSeriesData), StreamingHeaderData (StreamingHeaderData), StreamingImageData (StreamingImageData, frame, image, imageShape, realTimeNs, startTimeNs, stopTimeNs), StreamingMessage (StreamingEndOfSeries, StreamingHeader, StreamingImage), appendix, series, streamingLoop)
import Simplon.Util (fini, overwriteMVar, packShow, packShowLazy)
import System.IO (FilePath, IO)
import System.Log.FastLogger (LogStr, LogType' (LogStdout), ToLogStr (toLogStr), defaultBufSize, newTimeCache, withTimedFastLogger)
import Text.Show (Show)
import Web.Scotty (ActionM, get, json, jsonData, middleware, pathParam, put, scotty, status, text)
import Prelude (Float, Num ((*), (+), (-)), Ord ((<=)), RealFrac (round), error, realToFrac)

data EigerParameterValue
  = EigerValueFloat !Float
  | EigerValueBool !Bool
  | EigerValueText !Text
  | EigerValueInt !Int
  deriving (Show)

eigerParameterValueText :: EigerParameterValue -> Maybe Text
eigerParameterValueText (EigerValueText t) = Just t
eigerParameterValueText _ = Nothing

instance ToJSON EigerParameterValue where
  toJSON (EigerValueFloat f) = toJSON f
  toJSON (EigerValueBool f) = toJSON f
  toJSON (EigerValueText f) = toJSON f
  toJSON (EigerValueInt f) = toJSON f

data FloatOrInt = IsFloat !Float | IsInt !Int

instance ToJSON FloatOrInt where
  toJSON (IsFloat f) = toJSON f
  toJSON (IsInt f) = toJSON f

type EigerParameterLiteralType = Text

data AccessMode = ReadOnly | ReadWrite

instance ToJSON AccessMode where
  toJSON ReadOnly = "r"
  toJSON ReadWrite = "rw"

data EigerParameter a = EigerParameter
  { value :: a,
    valueType :: !EigerParameterLiteralType,
    accessMode :: !AccessMode,
    minValue :: !(Maybe FloatOrInt),
    maxValue :: !(Maybe FloatOrInt),
    unit :: !(Maybe Text),
    allowedValues :: ![Text]
  }

type MVarEigerParameter = EigerParameter (MVar EigerParameterValue)

instance ToJSON (EigerParameter EigerParameterValue) where
  toJSON (EigerParameter {value, valueType, accessMode, minValue, maxValue, unit, allowedValues}) =
    object $
      [ "value" .= value,
        "value_type" .= valueType,
        "access_mode" .= accessMode
      ]
        <> maybe [] (\v -> ["min" .= v]) minValue
        <> maybe [] (\v -> ["max" .= v]) maxValue
        <> maybe [] (\v -> ["unit" .= v]) unit
        <> ["allowed_values" .= allowedValues | not (null allowedValues)]

stateParameter :: MVar EigerParameterValue -> MVarEigerParameter
stateParameter var =
  EigerParameter
    { value = var,
      valueType = "string",
      accessMode = ReadOnly,
      minValue = Nothing,
      maxValue = Nothing,
      unit = Nothing,
      allowedValues = ["na", "idle", "ready", "acquire", "configure", "initialize", "error"]
    }

stateVariable :: MVar EigerParameterValue -> (Text, MVarEigerParameter)
stateVariable var =
  ( "state",
    stateParameter var
  )

type MVarEigerParameterMap = Map.Map Text MVarEigerParameter

initEigerStreamConfigParameters :: EigerConfig -> MVarEigerParameterMap
initEigerStreamConfigParameters c =
  Map.fromList
    [ stateVariable c.streamState,
      ( "mode",
        EigerParameter
          { value = c.streamEnabled,
            valueType = "string",
            accessMode = ReadWrite,
            minValue = Nothing,
            maxValue = Nothing,
            unit = Nothing,
            allowedValues = ["enabled", "disabled"]
          }
      ),
      ( "header_appendix",
        EigerParameter
          { value = c.streamHeaderAppendix,
            valueType = "string",
            accessMode = ReadWrite,
            minValue = Nothing,
            maxValue = Nothing,
            unit = Nothing,
            allowedValues = []
          }
      ),
      ( "image_appendix",
        EigerParameter
          { value = c.streamImageAppendix,
            valueType = "string",
            accessMode = ReadWrite,
            minValue = Nothing,
            maxValue = Nothing,
            unit = Nothing,
            allowedValues = []
          }
      )
    ]

initEigerStreamStatusParameters :: EigerConfig -> MVarEigerParameterMap
initEigerStreamStatusParameters c =
  Map.fromList [stateVariable c.streamState]

initEigerDetectorStatusParameters :: EigerConfig -> MVarEigerParameterMap
initEigerDetectorStatusParameters c =
  Map.fromList [stateVariable c.detectorState]

data EigerConfig = EigerConfig
  { nimages :: MVar EigerParameterValue,
    ntrigger :: MVar EigerParameterValue,
    triggerMode :: MVar EigerParameterValue,
    frameTime :: MVar EigerParameterValue,
    countTime :: MVar EigerParameterValue,
    fileWriterEnabled :: MVar EigerParameterValue,
    streamEnabled :: MVar EigerParameterValue,
    detectorState :: MVar EigerParameterValue,
    streamState :: MVar EigerParameterValue,
    fileWriterState :: MVar EigerParameterValue,
    streamHeaderAppendix :: MVar EigerParameterValue,
    streamImageAppendix :: MVar EigerParameterValue
  }

initialEigerConfig :: IO EigerConfig
initialEigerConfig =
  EigerConfig
    <$> newMVar (EigerValueInt 10)
    <*> newMVar (EigerValueInt 2)
    <*> newMVar (EigerValueText "ints")
    <*> newMVar (EigerValueFloat 0.5)
    <*> newMVar (EigerValueFloat 0.5)
    <*> newMVar (EigerValueText "disabled")
    <*> newMVar (EigerValueText "disabled")
    <*> newMVar (EigerValueText "idle")
    <*> newMVar (EigerValueText "idle")
    <*> newMVar (EigerValueText "idle")
    <*> newMVar (EigerValueText "")
    <*> newMVar (EigerValueText "")

initEigerDetectorConfigParameters :: EigerConfig -> MVarEigerParameterMap
initEigerDetectorConfigParameters c =
  Map.fromList
    [ ( "nimages",
        EigerParameter
          { value = c.nimages,
            valueType = "uint",
            accessMode = ReadWrite,
            minValue = Just (IsInt 1),
            maxValue = Just (IsInt 1_000_000),
            unit = Nothing,
            allowedValues = []
          }
      ),
      ( "ntrigger",
        EigerParameter
          { value = c.ntrigger,
            valueType = "uint",
            accessMode = ReadWrite,
            minValue = Just (IsInt 1),
            maxValue = Just (IsInt 1_000_000),
            unit = Nothing,
            allowedValues = []
          }
      ),
      ( "trigger_mode",
        EigerParameter
          { value = c.triggerMode,
            valueType = "string",
            accessMode = ReadWrite,
            minValue = Nothing,
            maxValue = Nothing,
            unit = Nothing,
            allowedValues =
              [ "eies",
                "exte",
                "extg",
                "exts",
                "inte",
                "ints"
              ]
          }
      ),
      ( "frame_time",
        EigerParameter
          { value = c.frameTime,
            valueType = "float",
            accessMode = ReadWrite,
            minValue = Just (IsFloat 0.003_571_429),
            maxValue = Nothing,
            unit = Just "s",
            allowedValues = []
          }
      ),
      ( "count_time",
        EigerParameter
          { value = c.countTime,
            valueType = "float",
            accessMode = ReadWrite,
            minValue = Just (IsFloat 0.003_571_429),
            maxValue = Just (IsFloat 3600.0),
            unit = Just "s",
            allowedValues = []
          }
      )
    ]

initEigerFileWriterStatusParameters :: EigerConfig -> MVarEigerParameterMap
initEigerFileWriterStatusParameters c =
  Map.fromList [stateVariable c.fileWriterState]

initEigerFileWriterConfigParameters :: EigerConfig -> MVarEigerParameterMap
initEigerFileWriterConfigParameters c =
  Map.fromList
    [ ( "mode",
        EigerParameter
          { value = c.fileWriterEnabled,
            valueType = "string",
            accessMode = ReadWrite,
            minValue = Nothing,
            maxValue = Nothing,
            unit = Nothing,
            allowedValues = ["enabled", "disabled"]
          }
      )
    ]

newtype SimplonPutRequest = SimplonPutRequest {value :: Value}

instance FromJSON SimplonPutRequest where
  parseJSON = withObject "SimplonPutRequest" $ \v -> SimplonPutRequest <$> (v .: "value")

modifyViaRequest :: SimplonPutRequest -> MVarEigerParameter -> IO ()
modifyViaRequest putRequest p = modifyMVar_ p.value \currentValue ->
  case p.accessMode of
    ReadOnly -> pure currentValue
    _ ->
      let newValue =
            case putRequest.value of
              String text' -> EigerValueText text'
              Number number ->
                case currentValue of
                  EigerValueFloat _ -> EigerValueFloat (realToFrac number)
                  EigerValueInt _ -> EigerValueInt (round number)
                  _ -> error "invalid parameter content for request"
              _ -> error "invalid request content"
       in pure newValue

data LoopConstantData = LoopConstantData
  { loopDataLog :: LogStr -> IO (),
    loopSignal :: TMVar LoopSignal,
    loopEigerConfig :: EigerConfig,
    loopStreamingConfig :: Maybe StreamingConfig
  }

data LoopSignal = Arm SeriesId | Trigger | Abort

waitForImageLoop :: LoopConstantData -> TriggerAndImageState -> Maybe Hdf5State -> IO ()
waitForImageLoop loopData@(LoopConstantData {loopDataLog, loopSignal, loopEigerConfig, loopStreamingConfig}) tais@(TriggerAndImageState {imageSeriesId, imageNtriggerLeft, imageFrameTime, imageNimagesLeft}) hdf5State = do
  loopDataLog $ packShow imageNimagesLeft <> " image(s) left, waiting for this one"
  delay <- registerDelay (round (imageFrameTime * 1000) * 1000)
  result <- atomically ((Just <$> takeTMVar loopSignal) <|> (Nothing <$ fini delay))
  case result of
    Nothing -> do
      for_ ((,) <$> loopStreamingConfig <*> hdf5State) \(streamingConfig', hdf5State') -> do
        imageAppendix <- readMVar loopEigerConfig.streamImageAppendix
        dimensions <- getDataSetDimensions hdf5State'.hdf5DataSet
        let imageWidth = dimensions !! 1
            imageHeight = dimensions !! 2
            currentFrame = determineCurrentFrame tais
        loopDataLog $ "current frame: " <> packShow currentFrame <> " opening and sending"
        withImage
          hdf5State'.hdf5DataSet
          -- hyperslab offset
          [currentFrame, 0, 0]
          -- count: size of one image
          [1, imageWidth, imageHeight]
          -- buffer size
          [imageWidth, imageHeight]
          \image -> do
            loopDataLog $ "read image: " <> packShow (BS.length image) <> " bytes"
            writeChan
              streamingConfig'.streamingChan
              ( StreamingImage
                  ( StreamingImageData
                      { image = image,
                        imageShape = dimensions,
                        series = imageSeriesId,
                        frame = currentFrame,
                        startTimeNs = 0,
                        stopTimeNs = 0,
                        realTimeNs = 0,
                        appendix = eigerParameterValueText imageAppendix
                      }
                  )
              )

      if imageNimagesLeft <= 1
        then do
          overwriteMVar loopEigerConfig.detectorState (EigerValueText "ready")
          overwriteMVar loopEigerConfig.streamState (EigerValueText "ready")
          if imageNtriggerLeft > 1
            then do
              loopDataLog $ "no images left, but " <> packShow (imageNtriggerLeft - 1) <> " trigger(s)"
              waitForTriggerLoop loopData (decreaseTriggerCount tais) hdf5State
            else do
              loopDataLog "no images left, no triggers left"
              overwriteMVar loopEigerConfig.detectorState (EigerValueText "idle")
              overwriteMVar loopEigerConfig.streamState (EigerValueText "ready")
        else do
          loopDataLog $ packShow (imageNimagesLeft - 1) <> " image(s) left"
          waitForImageLoop loopData (decreaseImageCount tais) hdf5State
    Just (Arm differentSeriesId) -> do
      loopDataLog $ "spurious arm received (series " <> packShow differentSeriesId <> "), ignoring..."
      waitForImageLoop loopData tais hdf5State
    Just Trigger -> do
      loopDataLog "spurious trigger received, ignoring..."
      waitForImageLoop loopData tais hdf5State
    Just Abort -> do
      loopDataLog "abort received"
      overwriteMVar loopEigerConfig.detectorState (EigerValueText "idle")
      overwriteMVar loopEigerConfig.streamState (EigerValueText "ready")

data StreamingConfig = StreamingConfig
  { streamingChan :: Chan StreamingMessage,
    streamingH5File :: FilePath,
    streamingH5DatasetPath :: Text
  }

data Hdf5State = Hdf5State
  { hdf5DataSet :: Hdf5DataSet,
    hdf5ImageNumber :: Int
  }

data TriggerAndImageState = TriggerAndImageState
  { imageSeriesId :: SeriesId,
    imageNtrigger :: Int,
    imageNtriggerLeft :: Int,
    imageFrameTime :: Float,
    imageNimagesTotal :: Int,
    imageNimagesLeft :: Int
  }

initialTriggerAndImageState :: SeriesId -> Int -> Int -> Float -> TriggerAndImageState
initialTriggerAndImageState seriesId ntrigger nimages frameTime =
  TriggerAndImageState
    { imageSeriesId = seriesId,
      imageNtrigger = ntrigger,
      imageNtriggerLeft = ntrigger,
      imageFrameTime = frameTime,
      imageNimagesTotal = nimages,
      imageNimagesLeft = nimages
    }

determineCurrentFrame :: TriggerAndImageState -> Int
determineCurrentFrame (TriggerAndImageState {imageNtrigger, imageNtriggerLeft, imageNimagesTotal, imageNimagesLeft}) =
  let triggersSoFar = imageNtrigger - imageNtriggerLeft
      imagesInThisTrigger = imageNimagesTotal - imageNimagesLeft
   in triggersSoFar * imageNimagesTotal + imagesInThisTrigger

decreaseTriggerCount :: TriggerAndImageState -> TriggerAndImageState
decreaseTriggerCount tais =
  -- after a successful trigger round, decrease trigger by 1 and increase images by the maximum amount
  tais
    { imageNtriggerLeft = tais.imageNtriggerLeft - 1,
      imageNimagesLeft = tais.imageNimagesTotal
    }

decreaseImageCount :: TriggerAndImageState -> TriggerAndImageState
decreaseImageCount tais = tais {imageNimagesLeft = tais.imageNimagesLeft - 1}

waitForTriggerLoop ::
  LoopConstantData ->
  TriggerAndImageState ->
  Maybe Hdf5State ->
  IO ()
waitForTriggerLoop loopData@(LoopConstantData {loopDataLog, loopSignal, loopEigerConfig}) triggerAndImageState hdf5State = do
  loopDataLog "waiting for trigger signal"
  signal <- atomically (takeTMVar loopSignal)
  case signal of
    Abort -> do
      loopDataLog "waited for trigger, but got abort"
      overwriteMVar loopEigerConfig.detectorState (EigerValueText "idle")
      overwriteMVar loopEigerConfig.streamState (EigerValueText "ready")
    Arm differentSeriesId ->
      loopDataLog $ "waited for trigger, but got arm (series " <> packShow differentSeriesId <> ") - ignoring..."
    Trigger -> do
      loopDataLog "got trigger signal, waiting for images"
      overwriteMVar loopEigerConfig.detectorState (EigerValueText "acquire")
      overwriteMVar loopEigerConfig.streamState (EigerValueText "acquire")
      waitForImageLoop loopData triggerAndImageState hdf5State

waitForArmLoop :: LoopConstantData -> IO ()
waitForArmLoop loopData@(LoopConstantData {loopDataLog, loopSignal, loopEigerConfig, loopStreamingConfig}) =
  forever do
    loopDataLog "waiting for arm signal"
    signal <- atomically (takeTMVar loopSignal)
    case signal of
      Abort -> loopDataLog "abort, but not triggering; ignoring"
      Arm seriesId -> do
        ntrigger <- readMVar loopEigerConfig.ntrigger
        nimages <- readMVar loopEigerConfig.nimages
        frameTime <- readMVar loopEigerConfig.frameTime

        case ntrigger of
          EigerValueInt ntrigger' ->
            case frameTime of
              EigerValueFloat frameTime' ->
                case nimages of
                  EigerValueInt nimages' -> do
                    overwriteMVar loopEigerConfig.detectorState (EigerValueText "ready")
                    overwriteMVar loopEigerConfig.streamState (EigerValueText "ready")
                    loopDataLog $
                      "armed, waiting for "
                        <> packShow ntrigger'
                        <> " trigger(s), "
                        <> packShow nimages'
                        <> " image(s)"

                    for_ loopStreamingConfig \streamingConfig' -> do
                      headerAppendix <- readMVar loopEigerConfig.streamHeaderAppendix
                      writeChan
                        streamingConfig'.streamingChan
                        ( StreamingHeader
                            ( StreamingHeaderData
                                { series = seriesId,
                                  appendix = eigerParameterValueText headerAppendix
                                }
                            )
                        )

                    case loopStreamingConfig of
                      Nothing ->
                        waitForTriggerLoop
                          loopData
                          (initialTriggerAndImageState seriesId ntrigger' nimages' frameTime')
                          Nothing
                      Just (StreamingConfig {streamingH5File, streamingH5DatasetPath}) ->
                        withHdf5FileAndDataSet streamingH5File streamingH5DatasetPath \hdf5File ->
                          waitForTriggerLoop
                            loopData
                            (initialTriggerAndImageState seriesId ntrigger' nimages' frameTime')
                            ( Just
                                ( Hdf5State
                                    { hdf5DataSet = hdf5File,
                                      hdf5ImageNumber = 0
                                    }
                                )
                            )

                    for_ loopStreamingConfig \streamingConfig' -> do
                      writeChan
                        streamingConfig'.streamingChan
                        ( StreamingEndOfSeries
                            ( StreamingEndOfSeriesData
                                { series = seriesId
                                }
                            )
                        )
                  _ -> loopDataLog "error reading nimages"
              _ -> loopDataLog "error reading frame time"
          _ -> loopDataLog "error reading ntrigger"
      Trigger -> loopDataLog "trigger received, ignoring"

main :: IO ()
main = do
  timeCache <- newTimeCache "%Y-%m-%dT%H:%M:%S%z"
  withOptions \options -> do
    withTimedFastLogger timeCache (LogStdout defaultBufSize) \fastLogger -> do
      let log msg = fastLogger (\time -> toLogStr time <> " " <> msg <> "\n")

      streamingChan <- newChan

      -- The loop runs forever until main stops, so we don't need the thread ID
      for_ options.zmqBindAddress \zmqBindAddress' ->
        void $
          forkIO $
            streamingLoop log zmqBindAddress' streamingChan

      let streamingConfig = const (StreamingConfig streamingChan options.inputH5File options.inputH5DatasetPath) <$> options.zmqBindAddress

      -- Test code to read 'n' images from the h5 file
      -- withHdf5FileAndDataSet options.inputH5File options.inputH5DatasetPath \ds -> do
      --   log "opened data set, reading from it now"
      --   dimensions <- getDataSetDimensions ds
      --   log $ "dimensions: " <> packShow dimensions
      --   forM_ [0 .. 10] \imageIndex -> do
      --     log $ "reading image " <> packShow imageIndex
      --     let imageWidth = dimensions !! 1
      --         imageHeight = dimensions !! 2
      --     withImage ds [imageIndex, 0, 0] [1, imageWidth, imageHeight] [imageWidth, imageHeight] \image -> do
      --       log $ "read image: " <> packShow (BS.length image) <> " bytes"

      eigerConfig <- initialEigerConfig
      let detectorStatusParams = initEigerDetectorStatusParameters eigerConfig
          detectorConfigParams = initEigerDetectorConfigParameters eigerConfig
          streamStatusParams = initEigerStreamStatusParameters eigerConfig
          streamConfigParams = initEigerStreamConfigParameters eigerConfig
          fileWriterStatusParams = initEigerFileWriterStatusParameters eigerConfig
          fileWriterConfigParams = initEigerFileWriterConfigParameters eigerConfig
      currentSeriesId :: MVar Int <- newMVar 0
      currentTriggerId :: MVar Int <- newMVar 0
      signalVar <- newEmptyTMVarIO
      log "starting arm loop"
      void $ forkIO $ waitForArmLoop (LoopConstantData log signalVar eigerConfig streamingConfig)
      let mvarMap :: Map.Map (Text, Text) MVarEigerParameterMap
          mvarMap =
            Map.fromList
              [ (("stream", "status"), streamStatusParams),
                (("stream", "config"), streamConfigParams),
                (("filewriter", "status"), fileWriterStatusParams),
                (("filewriter", "config"), fileWriterConfigParams),
                (("detector", "status"), detectorStatusParams),
                (("detector", "config"), detectorConfigParams)
              ]
          abort :: ActionM ()
          abort = liftIO $ atomically $ putTMVar signalVar Abort
          increaseSeriesId :: ActionM SeriesId
          increaseSeriesId = liftIO $ modifyMVar currentSeriesId (\currentId -> pure (currentId + 1, SeriesId (currentId + 1)))
          arm :: ActionM ()
          arm = do
            newSeriesId <- increaseSeriesId
            -- changeDetectorState "idle" "ready"
            liftIO $ modifyMVar_ currentTriggerId (const (pure 0))
            text (packShowLazy newSeriesId)
            liftIO $ atomically $ putTMVar signalVar (Arm newSeriesId)
          trigger = liftIO $ atomically $ putTMVar signalVar Trigger
          disarm = liftIO $ atomically $ putTMVar signalVar Abort
          commands :: Map.Map Text (ActionM ())
          commands =
            Map.fromList
              [ ("abort", abort),
                ("arm", arm),
                ("trigger", trigger),
                ("disarm", disarm)
              ]
      scotty options.listenPort do
        when options.accessLogging (middleware logStdoutDev)

        get "/detector/api/:version/command/keys" do
          json (Map.keys commands)
        put "/detector/api/:version/command/:commandname" do
          commandName <- pathParam "commandname"
          case Map.lookup commandName commands of
            Nothing -> status status404
            Just command -> command
        put "/:subsystem/api/:version/:configOrStatus/:parameter" do
          subsystem <- pathParam "subsystem"
          configOrStatus <- pathParam "configOrStatus"
          case Map.lookup (subsystem, configOrStatus) mvarMap of
            Nothing -> status status404
            Just parameterMap -> do
              parameter <- pathParam "parameter"
              requestContent <- jsonData
              case Map.lookup parameter parameterMap of
                Nothing -> status status404
                Just parameterDescription ->
                  liftIO $ modifyViaRequest requestContent parameterDescription

        get "/:subsystem/api/:version/:configOrStatus/keys" do
          subsystem <- pathParam "subsystem"
          configOrStatus <- pathParam "configOrStatus"
          case Map.lookup (subsystem, configOrStatus) mvarMap of
            Nothing -> status status404
            Just parameters -> json (Map.keys parameters)
        get "/:subsystem/api/:version/:configOrStatus/:parameter" do
          subsystem <- pathParam "subsystem"
          configOrStatus <- pathParam "configOrStatus"
          case Map.lookup (subsystem, configOrStatus) mvarMap of
            Nothing -> status status404
            Just parameterMap -> do
              parameter <- pathParam "parameter"
              case Map.lookup parameter parameterMap of
                Nothing -> status status404
                Just eigerParameter' -> do
                  resolvedValue <- liftIO $ readMVar eigerParameter'.value
                  let resolvedParameter :: EigerParameter EigerParameterValue
                      resolvedParameter = eigerParameter' {value = resolvedValue}
                  json resolvedParameter
