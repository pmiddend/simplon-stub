{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative (Applicative (pure))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Number, String), object, parseJSON, withObject, (.:))
import Data.Bool (Bool, not)
import Data.Eq ((==))
import Data.Foldable (Foldable (null), forM_, mapM_)
import Data.Function (const, ($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (status400, status404)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.IO (IO, putStrLn)
import Text.Show (Show (show))
import Web.Scotty (ActionM, finish, get, json, jsonData, middleware, pathParam, put, scotty, status, text)
import Prelude (Float, Num ((+), (-)), RealFrac (round), error, realToFrac)

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

data EigerParameter = EigerParameter
  { value :: !EigerParameterValue,
    valueType :: !EigerParameterLiteralType,
    accessMode :: !AccessMode,
    minValue :: !(Maybe FloatOrInt),
    maxValue :: !(Maybe FloatOrInt),
    unit :: !(Maybe Text),
    allowedValues :: ![Text]
  }

instance ToJSON EigerParameter where
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

boolParameter :: Bool -> AccessMode -> EigerParameter
boolParameter value accessMode =
  EigerParameter
    { value = EigerValueBool value,
      valueType = "bool",
      accessMode = accessMode,
      minValue = Nothing,
      maxValue = Nothing,
      unit = Nothing,
      allowedValues = []
    }

stateParameter :: Text -> EigerParameter
stateParameter value =
  EigerParameter
    { value = EigerValueText value,
      valueType = "string",
      accessMode = ReadOnly,
      minValue = Nothing,
      maxValue = Nothing,
      unit = Nothing,
      allowedValues = ["na", "idle", "ready", "acquire", "configure", "initialize", "error"]
    }

stateVariable :: (Text, EigerParameter)
stateVariable =
  ( "state",
    stateParameter "idle"
  )

initEigerStreamConfigParameters :: Map.Map Text EigerParameter
initEigerStreamConfigParameters =
  Map.fromList
    [ stateVariable,
      ( "mode",
        EigerParameter
          { value = EigerValueText "disabled",
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
          { value = EigerValueText "header appendix",
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
          { value = EigerValueText "image appendix",
            valueType = "string",
            accessMode = ReadWrite,
            minValue = Nothing,
            maxValue = Nothing,
            unit = Nothing,
            allowedValues = []
          }
      )
    ]

initEigerStreamStatusParameters :: Map.Map Text EigerParameter
initEigerStreamStatusParameters =
  Map.fromList [stateVariable]

initEigerDetectorStatusParameters :: Map.Map Text EigerParameter
initEigerDetectorStatusParameters =
  Map.fromList [stateVariable]

initEigerDetectorConfigParameters :: Map.Map Text EigerParameter
initEigerDetectorConfigParameters =
  Map.fromList
    [ ( "nimages",
        EigerParameter
          { value = EigerValueInt 10,
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
          { value = EigerValueInt 10,
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
          { value = EigerValueText "ints",
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
          { value = EigerValueFloat 0.01,
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
          { value = EigerValueFloat 0.005,
            valueType = "float",
            accessMode = ReadWrite,
            minValue = Just (IsFloat 0.003_571_429),
            maxValue = Just (IsFloat 3600.0),
            unit = Just "s",
            allowedValues = []
          }
      )
    ]

initEigerFileWriterStatusParameters :: Map.Map Text EigerParameter
initEigerFileWriterStatusParameters =
  Map.fromList [stateVariable]

initEigerFileWriterConfigParameters :: Map.Map Text EigerParameter
initEigerFileWriterConfigParameters =
  Map.fromList
    [ ( "nimages_per_file",
        EigerParameter
          { value = EigerValueInt 100,
            valueType = "uint",
            accessMode = ReadWrite,
            minValue = Nothing,
            maxValue = Nothing,
            unit = Nothing,
            allowedValues = []
          }
      ),
      ( "mode",
        EigerParameter
          { value = EigerValueText "disabled",
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

modifyViaRequest :: SimplonPutRequest -> EigerParameter -> EigerParameter
modifyViaRequest putRequest p =
  case p.accessMode of
    ReadOnly -> p
    _ ->
      let newValue =
            case putRequest.value of
              String text -> EigerValueText text
              Number number ->
                case p.value of
                  EigerValueFloat _ -> EigerValueFloat (realToFrac number)
                  EigerValueInt _ -> EigerValueInt (round number)
                  _ -> error "invalid parameter content for request"
              _ -> error "invalid request content"
       in p {value = newValue}

abort400 :: Text -> ActionM ()
abort400 message = do
  status status400
  json message
  finish

packShow :: (Show a) => a -> Text
packShow = pack . show

packShowLazy :: (Show a) => a -> TL.Text
packShowLazy = TL.pack . show

main :: IO ()
main = do
  detectorStatusParams <- newMVar initEigerDetectorStatusParameters
  detectorConfigParams <- newMVar initEigerDetectorConfigParameters
  streamStatusParams <- newMVar initEigerStreamStatusParameters
  streamConfigParams <- newMVar initEigerStreamConfigParameters
  fileWriterStatusParams <- newMVar initEigerFileWriterStatusParameters
  fileWriterConfigParams <- newMVar initEigerFileWriterConfigParameters
  currentSeriesId :: MVar Int <- newMVar 0
  currentTriggerId :: MVar Int <- newMVar 0
  let mvarMap :: Map.Map (Text, Text) (MVar (Map.Map Text EigerParameter))
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
      changeDetectorState from to = do
        detectorParams <- liftIO $ readMVar detectorStatusParams
        case Map.lookup "state" detectorParams of
          Nothing -> abort400 "couldn't find state in detector params"
          Just state ->
            case state.value of
              EigerValueText from' -> do
                if from' == from
                  then liftIO $ modifyMVar_ detectorStatusParams (pure . Map.insert "state" (stateParameter to))
                  else abort400 $ "detector's state is not \"" <> from <> "\" but \"" <> from' <> "\""
              _ -> abort400 "detector's state is not text"
      arm :: ActionM ()
      arm = do
        newSeriesId <- increaseSeriesId
        changeDetectorState "idle" "ready"
        liftIO $ modifyMVar_ currentTriggerId (const (pure 0))
        text (packShowLazy newSeriesId)
      trigger = do
        configParams <- liftIO $ readMVar detectorConfigParams

        case Map.lookup "nimages" configParams of
          Nothing -> status status404
          Just nimages' ->
            case nimages'.value of
              EigerValueInt nimages ->
                forM_ [0 .. nimages - 1] \i -> do
                  liftIO $ threadDelay 1000
              _ -> abort400 "no images to wait for"
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
        Just mvar -> do
          parameter <- pathParam "parameter"
          requestContent <- jsonData
          liftIO $ modifyMVar_ mvar (pure . Map.update (Just . modifyViaRequest requestContent) parameter)

    get "/:subsystem/api/:version/:configOrStatus/keys" do
      subsystem <- pathParam "subsystem"
      configOrStatus <- pathParam "configOrStatus"
      case Map.lookup (subsystem, configOrStatus) mvarMap of
        Nothing -> status status404
        Just mvar -> do
          paramsResolved <- liftIO $ readMVar mvar
          json (Map.keys paramsResolved)
    get "/:subsystem/api/:version/:configOrStatus/:parameter" do
      subsystem <- pathParam "subsystem"
      configOrStatus <- pathParam "configOrStatus"
      case Map.lookup (subsystem, configOrStatus) mvarMap of
        Nothing -> status status404
        Just mvar -> do
          parameter <- pathParam "parameter"
          paramsResolved <- liftIO $ readMVar mvar
          case Map.lookup parameter paramsResolved of
            Nothing -> do
              status status404
            Just paramValue -> do
              json paramValue
