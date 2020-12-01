{-# LANGUAGE DeriveGeneric #-} -- Language extention
{-# LANGUAGE DuplicateRecordFields #-} -- Using DuplicateRecordFields allows the records to use duplicate field labels.

module Parse  
  (-- | Used for parsing to and from JSON files into other data types.
    parse, parseParameters, -- Defining what needs to be exported
    Records (results),
    Parameters (results),
) where

import Data.Aeson -- To pass json data
import qualified Data.ByteString.Lazy.Char8 as BytStr
import GHC.Generics -- Will allow us to do the parsing automatically
import DataTypes -- Calling the module which defines the custom data types required for the data types 'Records'and 'Parameters'

-- | Custom data type for holding multiple rows of results
data Records = Records { 
  results :: [Record] -- ^ A list of type 'Record' (defined in the library DataTypes)
} deriving (Show, Generic) -- Making the new type an instance of the built in type classes
instance FromJSON Records -- Making our data type an instance of the type class FromJSON
instance ToJSON Records

-- | Custom data type for holding multiple parameters and their definitions
data Parameters = Parameters{ 
   results :: [Parameter]
} deriving (Show, Generic)
instance FromJSON Parameters
instance ToJSON Parameters

parse :: BytStr.ByteString -> Either String Records -- ^ The parse function could potentially fail, so either an error message is returned, or the desired records
parse json = eitherDecode json :: Either String Records 
parseParameters :: BytStr.ByteString -> Either String Parameters -- ^ The parseParameters function will either return an error message if there is a problem when parsing, or the desired records
parseParameters json = eitherDecode json :: Either String Parameters
