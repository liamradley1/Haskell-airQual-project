{-# LANGUAGE DeriveGeneric #-}

module Parse 
        ( parse,
            Coordinates(latitude, longitude),
            Date(utc, local),
            Reading(location, parameter, date, value, unit, coordinates, country, city),
            Results(results)
        ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics

data Coordinates = Coordinates {
    latitude :: Double,
    longitude :: Double
} deriving (Show, Generic)

instance FromJSON Coordinates
instance ToJSON Coordinates

data Date = Date {
    utc :: String,
    local :: String    
} deriving (Show, Generic)

instance FromJSON Date
instance ToJSON Date

data Reading = Reading {
            location :: String,
            parameter :: String,
            date :: Date,
            value :: Double,
            unit :: String,
            coordinates :: Coordinates,
            country :: String,
            city :: String
        } deriving (Show, Generic)

instance FromJSON Reading
instance ToJSON Reading

newtype Results = Results {
            results :: [Reading]
        } deriving (Show, Generic)

instance FromJSON Results
instance ToJSON Results

parse :: L8.ByteString -> Either String Results
parse json = eitherDecode json :: Either String Results