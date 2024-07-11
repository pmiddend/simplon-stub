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
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TVar, atomically, check, registerDelay)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Monad (forever, void, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (STM)
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
import Data.Foldable (Foldable (null))
import Data.Function (const, ($), (.))
import Data.Functor (Functor ((<$)), (<$>))
import Data.Int (Int)
import Data.List ((!!))
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Ord (Ord ((>)))
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Simplon.Hdf5 (getDataSetDimensions, withHdf5FileAndDataSet, withImage)
import Simplon.Options (accessLogging, inputH5DatasetPath, inputH5File, listenPort, withOptions)
import System.IO (IO)
import System.Log.FastLogger (LogStr, LogType' (LogStdout), ToLogStr (toLogStr), defaultBufSize, newTimeCache, withTimedFastLogger)
import Text.Show (Show (show))
import Web.Scotty (ActionM, get, json, jsonData, middleware, pathParam, put, scotty, status, text)
import Prelude (Float, Num ((*), (+), (-)), Ord ((<=)), RealFrac (round), error, realToFrac)

data EigerParameterValue
  = EigerValueFloat !Float
  | EigerValueBool !Bool
  | EigerValueText !Text
  | EigerValueInt !Int
  deriving (Show)

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

packShow :: (Show a) => a -> LogStr
packShow = toLogStr . show

packShowLazy :: (Show a) => a -> TL.Text
packShowLazy = TL.pack . show

data LoopSignal = Arm | Trigger | Abort

fini :: TVar Bool -> STM ()
fini = check <=< readTVar

overwriteMVar :: MVar a -> a -> IO ()
overwriteMVar var value = modifyMVar_ var (const (pure value))

waitForImageLoop :: (LogStr -> IO ()) -> TMVar LoopSignal -> Int -> Int -> Int -> Float -> EigerConfig -> IO ()
waitForImageLoop log signalVar ntriggerLeft nimagesTotal nimagesLeft frameTime eigerConfig = do
  log $ packShow nimagesLeft <> " image(s) left, waiting for this one"
  delay <- registerDelay (round (frameTime * 1000) * 1000)
  result <- atomically ((Just <$> takeTMVar signalVar) <|> (Nothing <$ fini delay))
  case result of
    Nothing ->
      if nimagesLeft <= 1
        then do
          overwriteMVar eigerConfig.detectorState (EigerValueText "ready")
          overwriteMVar eigerConfig.streamState (EigerValueText "ready")
          if ntriggerLeft > 1
            then do
              log $ "no images left, but " <> packShow (ntriggerLeft - 1) <> " trigger(s)"
              waitForTriggerLoop log signalVar (ntriggerLeft - 1) nimagesTotal frameTime eigerConfig
            else do
              log "no images left, no triggers left"
              overwriteMVar eigerConfig.detectorState (EigerValueText "idle")
              overwriteMVar eigerConfig.streamState (EigerValueText "ready")
        else do
          log $ packShow (nimagesLeft - 1) <> " image(s) left"
          waitForImageLoop log signalVar ntriggerLeft nimagesTotal (nimagesLeft - 1) frameTime eigerConfig
    Just Arm -> do
      log "spurious arm received, ignoring..."
      waitForImageLoop log signalVar ntriggerLeft nimagesTotal nimagesLeft frameTime eigerConfig
    Just Trigger -> do
      log "spurious trigger received, ignoring..."
      waitForImageLoop log signalVar ntriggerLeft nimagesTotal nimagesLeft frameTime eigerConfig
    Just Abort -> do
      log "abort received"
      overwriteMVar eigerConfig.detectorState (EigerValueText "idle")
      overwriteMVar eigerConfig.streamState (EigerValueText "ready")

waitForTriggerLoop :: (LogStr -> IO ()) -> TMVar LoopSignal -> Int -> Int -> Float -> EigerConfig -> IO ()
waitForTriggerLoop log signalVar ntrigger nimages frameTime eigerConfig = do
  log "waiting for trigger signal"
  signal <- atomically (takeTMVar signalVar)
  case signal of
    Abort -> do
      log "waited for trigger, but got abort"
      overwriteMVar eigerConfig.detectorState (EigerValueText "idle")
      overwriteMVar eigerConfig.streamState (EigerValueText "ready")
    Arm -> log "waited for trigger, but got arm - ignoring..."
    Trigger -> do
      log "got trigger signal, waiting for images"
      overwriteMVar eigerConfig.detectorState (EigerValueText "acquire")
      overwriteMVar eigerConfig.streamState (EigerValueText "acquire")
      waitForImageLoop log signalVar ntrigger nimages nimages frameTime eigerConfig

-- This implementation is wrong - we need to wait for triggers _and_ images (nimages vs. ntrigger)
-- This loop runs in parallel to the main server and initiates the triggering.
waitForArmLoop :: (LogStr -> IO ()) -> TMVar LoopSignal -> EigerConfig -> IO ()
waitForArmLoop log signalVar eigerConfig = forever do
  log "waiting for arm signal"
  signal <- atomically (takeTMVar signalVar)
  case signal of
    Abort -> log "abort, but not triggering; ignoring"
    Arm -> do
      ntrigger <- readMVar eigerConfig.ntrigger
      nimages <- readMVar eigerConfig.nimages
      frameTime <- readMVar eigerConfig.frameTime

      case ntrigger of
        EigerValueInt ntrigger' ->
          case frameTime of
            EigerValueFloat frameTime' ->
              case nimages of
                EigerValueInt nimages' -> do
                  overwriteMVar eigerConfig.detectorState (EigerValueText "ready")
                  overwriteMVar eigerConfig.streamState (EigerValueText "ready")
                  log $ "armed, waiting for " <> packShow ntrigger' <> " trigger(s), " <> packShow nimages' <> " image(s)"
                  waitForTriggerLoop log signalVar ntrigger' nimages' frameTime' eigerConfig
                _ -> log "error reading nimages"
            _ -> log "error reading frame time"
        _ -> log "error reading ntrigger"
    Trigger -> log "trigger received, ignoring"

main :: IO ()
main = do
  timeCache <- newTimeCache "%Y-%m-%dT%H:%M:%S%z"
  withOptions \options -> do
    withTimedFastLogger timeCache (LogStdout defaultBufSize) \fastLogger -> do
      let log msg = fastLogger (\time -> toLogStr time <> " " <> msg <> "\n")

      withHdf5FileAndDataSet options.inputH5File options.inputH5DatasetPath \ds -> do
        log "opened data set, reading from it now"
        dimensions <- getDataSetDimensions ds
        log $ "dimensions: " <> packShow dimensions
        let imageWidth = dimensions !! 1
            imageHeight = dimensions !! 2
        withImage ds [0, 0, 0] [1, imageWidth, imageHeight] [imageWidth, imageHeight] \image -> do
          log $ "read image: " <> packShow (BS.length image) <> " bytes"

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
      void $ forkIO (waitForArmLoop log signalVar eigerConfig)
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
          increaseSeriesId :: ActionM Int
          increaseSeriesId = liftIO $ modifyMVar currentSeriesId (\currentId -> pure (currentId + 1, currentId + 1))
          arm :: ActionM ()
          arm = do
            newSeriesId <- increaseSeriesId
            -- changeDetectorState "idle" "ready"
            liftIO $ modifyMVar_ currentTriggerId (const (pure 0))
            text (packShowLazy newSeriesId)
            liftIO $ atomically $ putTMVar signalVar Arm
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
