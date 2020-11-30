{-# LANGUAGE DeriveGeneric #-}
module DataTypes (Record(location, city, country, measurements, coordinates), Parameter(id, name, description, preferredUnit), Measurement(parameter, value, lastUpdated, unit), Coordinates(latitude, longitude)) where

import Data.Aeson
import GHC.Generics

data Measurement = Measurement { -- Custom data type definition
  parameter :: String,
  value :: Double,
  lastUpdated :: String,
  unit :: String
} deriving (Show, Generic) -- Making the new type an instance of the built in type classes
instance FromJSON Measurement
instance ToJSON Measurement

data Coordinates = Coordinates {
  latitude :: Double,
  longitude :: Double
} deriving (Show, Generic)
instance FromJSON Coordinates
instance ToJSON Coordinates

data Record = Record {
  location :: String,
  city :: String,
  country :: String,
  measurements :: [Measurement],
  coordinates :: Coordinates
} deriving (Show, Generic)
instance FromJSON Record
instance ToJSON Record

data Parameter = Parameter{
  id :: String,
  name :: String,
  description :: String,
  preferredUnit :: String
}deriving (Show, Generic)
instance FromJSON Parameter
instance ToJSON Parameter
