module Simplon.EigerParameterNameValue (EigerParameterNameValue (..)) where

import Data.Text (Text)
import Simplon.EigerParameterValue (EigerParameterValue)
import Text.Show (Show)
import Prelude ()

data EigerParameterNameValue = EigerParameterNameValue
  { name :: !Text,
    value :: !EigerParameterValue
  }
  deriving (Show)
