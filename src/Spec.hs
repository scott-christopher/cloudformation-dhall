{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Spec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Map
import Data.Maybe
import Data.List as L
import Data.Text as T
import GHC.Generics

prefixedField :: String -> String -> String
prefixedField field = fromJust . L.stripPrefix field

prefixedOptions :: String -> Options
prefixedOptions prefix = defaultOptions
  { fieldLabelModifier = prefixedField prefix
  }

data Spec = Spec
  { specPropertyTypes :: Map Text PropertyTypes
  , specResourceTypes :: Map Text ResourceSpec
  , specResourceSpecificationVersion :: Text
  } deriving (Generic, Show)

instance FromJSON Spec where
  parseJSON = genericParseJSON $ prefixedOptions "spec"

data PropertyTypes = PropertyTypes
  { propTypeDocumentation :: Maybe Text
  , propTypeProperties    :: Maybe (Map Text PropertySpec)
  } deriving (Generic, Show)

instance FromJSON PropertyTypes where
  parseJSON = genericParseJSON $ prefixedOptions "propType"

data PropertySpec = PropertySpec
  { propDocumentation :: Text
  , propRequired      :: Bool
  , propUpdateType    :: UpdateType
  , propType          :: PropertyType
  } deriving (Generic, Show)

instance FromJSON PropertySpec where
  parseJSON = withObject "PropertySpec" $ \v -> PropertySpec
      <$> v .: "Documentation"
      <*> v .: "Required"
      <*> v .: "UpdateType"
      <*> parseType v

parseType :: Object -> Parser PropertyType
parseType v = asum [ v .: "Type" >>= parsePropType v
                   , PrimProp <$> v .: "PrimitiveType" 
                   , v .: "PrimitiveType" >>= parseMapPrim
                   ]

-- workaround for "PrimitiveType": "Map" found in AWS::ServiceDiscovery::Instance.Properties.InstanceAttributes
parseMapPrim :: Text -> Parser PropertyType
parseMapPrim "Map" = pure $ MapProp (PrimProp PString)
parseMapPrim _     = fail "Not a Primitive Map"

parsePropType :: Object -> Text -> Parser PropertyType
parsePropType v "List" = ListProp <$> v .:? "DuplicatesAllowed" .!= False <*> itemType v 
parsePropType v "Map"  = MapProp <$> itemType v
parsePropType _ t      = pure $ ResourceProp t

itemType :: Object -> Parser PropertyType 
itemType v = ResourceProp <$> v .: "ItemType"
         <|> PrimProp <$> v .: "PrimitiveItemType"

data PropertyType = ListProp Bool PropertyType
                  | MapProp PropertyType
                  | ResourceProp Text
                  | PrimProp PrimType
  deriving (Generic, Show)

instance FromJSON PropertyType where
  parseJSON = withObject "PropertyType" parseType

data PrimType = PString | PLong | PInteger | PDouble | PBoolean | PTimestamp | PJson
  deriving (Generic, Show)

primTypeFromText :: Text -> Parser PrimType
primTypeFromText "String"    = pure PString
primTypeFromText "Long"      = pure PLong
primTypeFromText "Integer"   = pure PInteger
primTypeFromText "Double"    = pure PDouble
primTypeFromText "Boolean"   = pure PBoolean
primTypeFromText "Timestamp" = pure PTimestamp
primTypeFromText "Json"      = pure PJson
primTypeFromText _           = fail "invalid PrimitiveType"

instance FromJSON PrimType where
  parseJSON = withText "PrimitiveType" primTypeFromText

data UpdateType = Mutable | Immutable | Conditional
  deriving (Generic, Show)

instance FromJSON UpdateType

data ResourceSpec = ResourceSpec
  { resourceAttributes :: Maybe (Map Text PropertyType)
  , resourceDocumentation :: Text
  , resourceProperties :: Map Text PropertySpec
  } deriving (Generic, Show)

instance FromJSON ResourceSpec where
  parseJSON = genericParseJSON $ prefixedOptions "resource"

