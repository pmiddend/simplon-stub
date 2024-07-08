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
import Control.Concurrent (ThreadId, newEmptyMVar, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TVar, atomically, check, orElse, readTVarIO, registerDelay)
import Control.Concurrent.STM.TMVar (TMVar, takeTMVar)
import Control.Concurrent.STM.TVar (readTVar)
import Control.Monad (forever, void, (<=<))
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
import Data.Bool (Bool (False), not)
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (Foldable (null), forM_)
import Data.Function (const, ($), (.))
import Data.Functor (Functor ((<$)), (<$>))
import Data.Int (Int)
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (status400, status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.IO (IO)
import Text.Show (Show (show))
import Web.Scotty (ActionM, finish, get, json, jsonData, middleware, pathParam, put, scotty, status, text)
import Prelude (Float, Num ((*), (+), (-)), RealFrac (round), error, realToFrac)

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
          { value = c.fileWriterEnabled,
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
    <*> newMVar (EigerValueInt 1)
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
          { value = c.nimages,
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

abort400 :: Text -> ActionM ()
abort400 message = do
  status status400
  json message
  finish

packShow :: (Show a) => a -> Text
packShow = pack . show

packShowLazy :: (Show a) => a -> TL.Text
packShowLazy = TL.pack . show

data LoopSignal = Arm | Trigger | Abort

fini :: TVar Bool -> STM ()
fini = check <=< readTVar

-- This implementation is wrong - we need to wait for triggers _and_ images (nimages vs. ntrigger)
triggerLoop :: TMVar LoopSignal -> Int -> Float -> EigerConfig -> IO ()
triggerLoop _ 0 _ _ = do
  putStrLn "last trigger, quitting now"
triggerLoop signalVar ntrigger frameTime eigerConfig = do
  delay <- registerDelay (round (frameTime * 1000) * 1000)
  result <- atomically ((Just <$> takeTMVar signalVar) <|> (Nothing <$ fini delay))
  case result of
    Nothing -> triggerLoop signalVar (ntrigger - 1) frameTime eigerConfig
    Just Arm -> do
      putStrLn "spurious arm received, ignoring..."
      triggerLoop signalVar ntrigger frameTime eigerConfig
    Just Abort ->
      putStrLn "abort received, aborting..."
    Just Trigger -> do
      putStrLn
        "premature trigger, but counting anyways"
      triggerLoop signalVar (ntrigger - 1) frameTime eigerConfig

overwriteMVar var value = modifyMVar_ var (const (pure value))

armAndTriggerLoop :: TMVar LoopSignal -> EigerConfig -> IO ()
armAndTriggerLoop signalVar eigerConfig = forever do
  putStrLn "waiting for signal"
  signal <- atomically (takeTMVar signalVar)
  case signal of
    Abort -> putStrLn "abort, but not triggering; ignoring"
    Arm -> do
      ntrigger <- readMVar eigerConfig.nimages
      frameTime <- readMVar eigerConfig.frameTime

      case ntrigger of
        EigerValueInt ntrigger' ->
          case frameTime of
            EigerValueFloat frameTime' -> do
              putStrLn $ "armed, waiting for " <> packShow ntrigger <> " trigger(s)"
              triggerLoop signalVar ntrigger' frameTime' eigerConfig
            _ -> putStrLn $ "error reading frame time"
        _ -> putStrLn "error reading ntrigger"
    Trigger -> do
      putStrLn "trigger received"
      nimages <- readMVar eigerConfig.nimages
      ntrigger <- readMVar eigerConfig.nimages
      frameTime <- readMVar eigerConfig.frameTime

      overwriteMVar eigerConfig.detectorState (EigerValueText "acquire")
      overwriteMVar eigerConfig.streamState (EigerValueText "acquire")

      overwriteMVar eigerConfig.detectorState (EigerValueText "idle")
      overwriteMVar eigerConfig.streamState (EigerValueText "idle")

-- now wait for abort signal or thread delay

main :: IO ()
main = do
  eigerConfig <- initialEigerConfig
  let detectorStatusParams = initEigerDetectorStatusParameters eigerConfig
      detectorConfigParams = initEigerDetectorConfigParameters eigerConfig
      streamStatusParams = initEigerStreamStatusParameters eigerConfig
      streamConfigParams = initEigerStreamConfigParameters eigerConfig
      fileWriterStatusParams = initEigerFileWriterStatusParameters eigerConfig
      fileWriterConfigParams = initEigerFileWriterConfigParameters eigerConfig
  currentSeriesId :: MVar Int <- newMVar 0
  currentTriggerId :: MVar Int <- newMVar 0
  currentTriggeringLoop :: MVar ThreadId <- newEmptyMVar
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
      abort = json ("lol" :: Text)
      increaseSeriesId :: ActionM Int
      increaseSeriesId = liftIO $ modifyMVar currentSeriesId (\currentId -> pure (currentId + 1, currentId + 1))
      changeDetectorState :: Text -> Text -> ActionM ()
      changeDetectorState from to = do
        currentState' <- liftIO $ readMVar eigerConfig.detectorState
        case currentState' of
          EigerValueText currentState ->
            if currentState /= from
              then abort400 $ "detector's state is not \"" <> from <> "\" but \"" <> from <> "\""
              else liftIO $ void $ modifyMVar_ eigerConfig.detectorState (\_ -> pure (EigerValueText to))
          _ -> abort400 "detector's state is not text"
      arm :: ActionM ()
      arm = do
        newSeriesId <- increaseSeriesId
        changeDetectorState "idle" "ready"
        liftIO $ modifyMVar_ currentTriggerId (const (pure 0))
        text (packShowLazy newSeriesId)
      -- FIXME: needs to use signalVar to signal triggering
      trigger = pure ()
      disarm = do
        changeDetectorState "ready" "idle"
        currentSeriesId' <- liftIO $ readMVar currentSeriesId
        text (packShowLazy currentSeriesId')
      commands :: Map.Map Text (ActionM ())
      commands =
        Map.fromList
          [ ("abort", abort),
            ("arm", arm),
            ("trigger", trigger),
            ("disarm", disarm)
          ]
  scotty 10_001 do
    middleware logStdoutDev

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
