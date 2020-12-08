{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : DataTypes
Description : Provides a set of data types that are utilised within the Parse and Database modules.

Provides a set of data types that are utilised within the Parse and Database modules.
-}
module DataTypes (
  Record(location, city, country, measurements, coordinates),
  Parameter(id, name, description, preferredUnit),
  Measurement(parameter, value, lastUpdated, unit), 
  Coordinates(latitude, longitude)) where 

import Data.Aeson
import GHC.Generics

-- | Custom data type definition for measurements
data Measurement = Measurement { 
  parameter :: String,
  value :: Double,
  lastUpdated :: String,
  unit :: String
} deriving (Show -- ^ Using default implementation for Show
  , Generic -- ^ Using default implementation for Generic
  )
-- | Allow a Measurement to be instantiated from a JSON representation
instance FromJSON Measurement
-- | Allows JSON representations to be created from a Measurement
instance ToJSON Measurement

-- | Custom data type definition for coordinates
data Coordinates = Coordinates { 
  latitude :: Double,
  longitude :: Double
} deriving (Show -- ^ Using default implementation for Show
  , Generic -- ^ Using default implementation for Generic
  ) 
-- | Allow a set of Coordinates to be instantiated from a JSON representation
instance FromJSON Coordinates
-- | Allows JSON representations to be created from a set of Coordinates 
instance ToJSON Coordinates

-- | Custom data type definition for records
data Record = Record { 
  location :: String,
  city :: String,
  country :: String,
  measurements :: [Measurement],
  coordinates :: Coordinates
} deriving (Show -- ^ Using default implementation for Show
  , Generic -- ^ Using default implementation for Generic
 ) 
-- | Allows a Record to be instantiated from a JSON representation
instance FromJSON Record  
-- | Allows JSON representations to be created from a Record
instance ToJSON Record 

-- | Custom data type definition for parameters
data Parameter = Parameter{ 
  id :: String,
  name :: String,
  description :: String,
  preferredUnit :: String
} deriving (Show -- ^ Using default implementation for Show
  , Generic -- ^ Using default implementation for Generic
  ) 
-- | Allows a Parameter to be instantiated from a JSON representation
instance FromJSON Parameter
-- | Allows JSON representations to be created from a Parameter 
instance ToJSON Parameter 
