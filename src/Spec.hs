{-# LANGUAGE OverloadedStrings #-}

module Spec where

import Control.Applicative  ( (<|>) )
import Data.Aeson           ( FromJSON, Object
                            , parseJSON, withObject, withText
                            , (.:), (.:?), (.!=)
                            )
import Data.Aeson.Types     ( Parser )
import Data.Map             ( Map
                            , empty, singleton )
import Data.Text            ( Text )

-----------------------------------------------

data Spec = Spec
  { specPropertyTypes :: Map Text PropertyTypes
  , specResourceTypes :: Map Text ResourceSpec
  , specResourceSpecificationVersion :: Text
  } deriving (Show)

data PropertyTypes = PropertyTypes
  { propTypeDocumentation :: Text
  , propTypeProperties    :: Map Text PropertySpec
  } deriving (Show)

data IsRequired = Required | Optional | RequiredUndefined
  deriving (Show)

data PropertySpec = PropertySpec
  { propDocumentation :: Text
  , propRequired      :: IsRequired
  , propUpdateType    :: UpdateType
  , propType          :: PropertyType
  } deriving (Show)

data PropertyType = ListProp Bool PropertyType
                  | MapProp PropertyType
                  | ResourceProp Text
                  | PrimProp PrimType
  deriving (Show)

data PrimType = PString | PLong | PInteger | PDouble | PBoolean | PTimestamp | PJson
  deriving (Show)

data UpdateType = Mutable | Immutable | Conditional | UpdateTypeUndefined
  deriving (Show)

data ResourceSpec = ResourceSpec
  { resourceAttributes :: Map Text PropertyType
  , resourceDocumentation :: Text
  , resourceProperties :: Map Text PropertySpec
  } deriving (Show)

-----------------------------------------------

isRequired :: Bool -> IsRequired
isRequired True  = Required
isRequired False = Optional

parseType :: Object -> Parser PropertyType
parseType v = (v .: "Type" >>= parsePropType v)
          <|> (PrimProp <$> v .: "PrimitiveType")
          <|> (v .: "PrimitiveType" >>= parseMapPrim)
            where
              parseMapPrim :: Text -> Parser PropertyType -- workaround for "PrimitiveType": "Map"
              parseMapPrim "Map" = pure $ MapProp (PrimProp PString)
              parseMapPrim _     = fail "Not a Primitive Map"

parsePropType :: Object -> Text -> Parser PropertyType
parsePropType v "List" = ListProp <$> v .:? "DuplicatesAllowed" .!= False <*> itemType v 
parsePropType v "Map"  = MapProp <$> itemType v
parsePropType _ t      = pure $ ResourceProp t

itemType :: Object -> Parser PropertyType 
itemType v = ResourceProp <$> v .: "ItemType"
         <|> PrimProp <$> v .: "PrimitiveItemType"

-----------------------------------------------

instance FromJSON Spec where
  parseJSON = withObject "Spec" $ \v -> Spec
    <$> v .: "PropertyTypes"
    <*> v .: "ResourceTypes"
    <*> v .: "ResourceSpecificationVersion"

instance FromJSON PropertyTypes where
  parseJSON = withObject "PropertyTypes" $ \v ->
    PropertyTypes <$> v .: "Documentation" <*> v .: "Properties"
    <|> -- Workaround for AWS::EC2::LaunchTemplate.CapacityReservationPreference
    PropertyTypes "" <$> (singleton "" . PropertySpec "" RequiredUndefined UpdateTypeUndefined <$> parseType v)

instance FromJSON PropertyType where
  parseJSON = withObject "PropertyType" parseType

instance FromJSON PrimType where
  parseJSON = withText "PrimitiveType" primTypeFromText
    where
      primTypeFromText :: Text -> Parser PrimType
      primTypeFromText "String"    = pure PString
      primTypeFromText "Long"      = pure PLong
      primTypeFromText "Integer"   = pure PInteger
      primTypeFromText "Double"    = pure PDouble
      primTypeFromText "Boolean"   = pure PBoolean
      primTypeFromText "Timestamp" = pure PTimestamp
      primTypeFromText "Json"      = pure PJson
      primTypeFromText _           = fail "invalid PrimitiveType"

instance FromJSON UpdateType where
  parseJSON = withText "UpdateType" (pure . parseUpdateType)
    where
      parseUpdateType "Mutable"     = Mutable
      parseUpdateType "Immutable"   = Immutable
      parseUpdateType "Conditional" = Conditional
      parseUpdateType _             = UpdateTypeUndefined

instance FromJSON PropertySpec where
  parseJSON = withObject "PropertySpec" $ \v -> PropertySpec
      <$> v .: "Documentation"
      <*> (isRequired <$> v .: "Required")
      <*> v .: "UpdateType"
      <*> parseType v

instance FromJSON ResourceSpec where
  parseJSON = withObject "ResourceSpec" $ \v -> ResourceSpec
    <$> v .:? "Attributes" .!= Data.Map.empty
    <*> v .: "Documentation"
    <*> v .: "Properties"

