module HTTP
(download) where -- Exporting the download function

import qualified Data.ByteString.Lazy.Char8 as BytStr --Importing the libary to get ByteString
import Network.HTTP.Simple

type URL = String --Type synonym
download :: URL -> IO BytStr.ByteString --Download takes in a URL (String) and returns a IO ByteString
download url = do -- Defining the function to download the data
    request <- parseRequest url --Takes the url and returns an IO of a request
    response <- httpLBS request --Makes the HTTP request
    return $ getResponseBody response --Returns, to turn ByteString into an IO