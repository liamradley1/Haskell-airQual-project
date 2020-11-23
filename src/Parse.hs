{-# LANGUAGE DeriveGeneric #-} -- Language extention
{-# LANGUAGE DuplicateRecordFields #-} -- Using DuplicateRecordFields allows the records to use duplicate field labels.

module Parse
  (parse, parseParameters, -- Defining what needs to be exported
    Records (results),
    Parameters (results)
) where

import Data.Aeson -- To pass json data
import qualified Data.ByteString.Lazy.Char8 as BytStr
import GHC.Generics -- Will allow us to do the parsing automatically
import DataTypes -- Calling the module which defines the custom data types required for the data types 'Records'and 'Parameters'

data Records = Records { -- Custom data type for holding each row of the results
  results :: [Record] -- A list of type 'Record' (defined in the library DataTypes)
} deriving (Show, Generic) -- Making the new type an instance of the built in type classes
instance FromJSON Records -- Making our data type an instance of the type class FromJSON
instance ToJSON Records

data Parameters = Parameters{ -- Custom data type for holding a list of 'Parameter' and their definitions
   results :: [Parameter]
} deriving (Show, Generic)
instance FromJSON Parameters
instance ToJSON Parameters

parse :: BytStr.ByteString -> Either String Records --  Type declaration of the parse function
parse json = eitherDecode json :: Either String Records -- Defining our parse function and as parsing could potentially fail we are returning either the error message or the Records
parseParameters :: BytStr.ByteString -> Either String Parameters
parseParameters json = eitherDecode json :: Either String Parameters






-- module Parse
--         ( parse,
--             Coordinates(latitude, longitude),
--             Date(utc, local),
--             Reading(location, parameter, date, value, unit, coordinates, country, city),
--             Results(results)
--         ) where



-- import Data.Aeson
-- import qualified Data.ByteString.Lazy.Char8 as L8
-- import GHC.Generics

-- data Coordinates = Coordinates {
--     latitude :: Double,
--     longitude :: Double
-- } deriving (Show, Generic)
--
-- instance FromJSON Coordinates
-- instance ToJSON Coordinates
--
-- data Date = Date {
--     utc :: String,
--     local :: String
-- } deriving (Show, Generic)
--
-- instance FromJSON Date
-- instance ToJSON Date
--
-- data Reading = Reading {
--             location :: String,
--             parameter :: String,
--             date :: Date,
--             value :: Double,
--             unit :: String,
--             coordinates :: Coordinates,
--             country :: String,
--             city :: String
--         } deriving (Show, Generic)
--
-- instance FromJSON Reading
-- instance ToJSON Reading
--
-- newtype Results = Results {
--             results :: [Reading]
--         } deriving (Show, Generic)
--
-- instance FromJSON Results
-- instance ToJSON Results

-- parse :: L8.ByteString -> Either String Results
-- parse json = eitherDecode json :: Either String Results
