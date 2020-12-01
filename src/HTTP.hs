{-|
Module      : HTTP
Description : Used for making HTTP requests to a REST API, and for downloafing of the contents from this API call.

Used for making HTTP requests to a REST API, and for downloafing of the contents from this API call.
-}
module HTTP 
(
    download, URL) where -- Exporting the download function

import qualified Data.ByteString.Lazy.Char8 as BytStr -- Importing the libary to get ByteString
import Network.HTTP.Simple

-- | Type synonym to denote a URL
type URL = String 

-- | Download takes in a URL (String) and returns a IO ByteString of JSON data
download :: URL -> IO BytStr.ByteString 
download url = do -- Defining the function to download the data
    request <- parseRequest url --Takes the url and returns an IO of a request
    response <- httpLBS request --Makes the HTTP request
    return $ getResponseBody response --Returns, to turn ByteString into an IO