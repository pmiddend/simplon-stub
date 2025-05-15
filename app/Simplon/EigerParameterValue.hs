module Simplon.EigerParameterValue (EigerParameterValue (..), eigerParameterValueText) where

import Data.Aeson (ToJSON, toJSON)
import Data.Bool (Bool)
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text (Text)
import Text.Show (Show)
import Prelude (Float)

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
