{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Number, String), object, parseJSON, withObject, (.:))
import qualified Data.Map as Map
import Data.Text (Text)
import Network.HTTP.Types.Status (status404)
import Web.Scotty (ActionM, get, json, jsonData, pathParam, put, scotty, status)

data EigerParameterValue
  = EigerValueFloat Float
  | EigerValueBool Bool
  | EigerValueText Text
  | EigerValueInt Int

instance ToJSON EigerParameterValue where
  toJSON (EigerValueFloat f) = toJSON f
  toJSON (EigerValueBool f) = toJSON f
  toJSON (EigerValueText f) = toJSON f
  toJSON (EigerValueInt f) = toJSON f

data FloatOrInt = IsFloat Float | IsInt Int

instance ToJSON FloatOrInt where
  toJSON (IsFloat f) = toJSON f
  toJSON (IsInt f) = toJSON f

type EigerParameterLiteralType = Text

data AccessMode = ReadOnly | ReadWrite

instance ToJSON AccessMode where
  toJSON ReadOnly = "r"
  toJSON ReadWrite = "rw"

data EigerParameter = EigerParameter
  { value :: EigerParameterValue,
    valueType :: EigerParameterLiteralType,
    accessMode :: AccessMode,
    minValue :: Maybe FloatOrInt,
    maxValue :: Maybe FloatOrInt,
    unit :: Maybe Text,
    allowedValues :: [Text]
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

initEigerStatusParameters :: Map.Map Text EigerParameter
initEigerStatusParameters =
  Map.fromList
    [ ( "state",
        EigerParameter
          { value = EigerValueText "idle",
            valueType = "string",
            accessMode = ReadOnly,
            minValue = Nothing,
            maxValue = Nothing,
            unit = Nothing,
            allowedValues = ["na", "idle", "ready", "acquire", "confgiure", "initialize", "error"]
          }
      )
    ]

initEigerStreamParameters :: Map.Map Text EigerParameter
initEigerStreamParameters =
  Map.fromList
    [ ( "mode",
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

initEigerConfigParameters :: Map.Map Text EigerParameter
initEigerConfigParameters =
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
            minValue = Just (IsFloat 0.003571429),
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
            minValue = Just (IsFloat 0.003571429),
            maxValue = Just (IsFloat 3600.0),
            unit = Just "s",
            allowedValues = []
          }
      )
    ]

initEigerFileWriterParameters :: Map.Map Text EigerParameter
initEigerFileWriterParameters =
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

main :: IO ()
main = do
  configParams <- newMVar initEigerConfigParameters
  streamParams <- newMVar initEigerStreamParameters
  fileWriterParams <- newMVar initEigerFileWriterParameters
  let mvarMap :: Map.Map Text (MVar (Map.Map Text EigerParameter))
      mvarMap = Map.fromList [("config", configParams), ("stream", streamParams), ("filewriter", fileWriterParams)]
      abort = json ("lol" :: Text)
      commands :: Map.Map Text (ActionM ())
      commands =
        Map.fromList
          [ ("abort", abort)
          ]
  scotty 3000 do
    put "/detector/api/:version/:subsystem/:parameter" do
      subsystem <- pathParam "subsystem"
      case Map.lookup subsystem mvarMap of
        Nothing -> status status404
        Just mvar -> do
          parameter <- pathParam "parameter"
          requestContent <- jsonData
          liftIO $ modifyMVar_ mvar (pure . Map.update (Just . modifyViaRequest requestContent) parameter)

    get "/detector/api/:version/command/keys" do
      json (Map.keys commands)
    get "/detector/api/:version/command/:commandname" do
      commandName <- pathParam "commandname"
      case Map.lookup commandName commands of
        Nothing -> status status404
        Just command -> command
    get "/detector/api/:version/:subsystem/keys" do
      subsystem <- pathParam "subsystem"
      case Map.lookup subsystem mvarMap of
        Nothing -> status status404
        Just mvar -> do
          paramsResolved <- liftIO $ readMVar mvar
          json (Map.keys paramsResolved)
    get "/detector/api/:version/:subsystem/:parameter" do
      subsystem <- pathParam "subsystem"
      case Map.lookup subsystem mvarMap of
        Nothing -> status status404
        Just mvar -> do
          parameter <- pathParam "parameter"
          paramsResolved <- liftIO $ readMVar mvar
          case Map.lookup parameter paramsResolved of
            Nothing -> do
              status status404
            Just paramValue -> do
              json paramValue
